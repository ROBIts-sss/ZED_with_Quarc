/*
    The Quanser Communications (QCOMM) API defines a generic unbuffered
    communication API. The communication protocol and channel used is
    identified by a URI. This API is general and extensible to other
    protocols simply by adding another protocol dynamic link library
    named qrt_<protocol name> and obeying the interface defined in the
    communications_protocol.h header file. Note that all the functions 
    in a protocol library must be prefixed by "<protocol name>_". For
    example, the functions in the TCP/IP library are prefixed by "tcpip_".

    For a buffered communications API, refer to quanser_stream.h.
*/
#if !defined(_quanser_communications_h)
#define _quanser_communications_h

#include "quanser_types.h"
#include "quanser_time.h"
#include "quanser_extern.h"
#include "quanser_errors.h"

typedef struct tag_connection * t_connection;

/* Flags used by the qcomm_poll function */
#define QCOMM_POLL_RECEIVE  0x01
#define QCOMM_POLL_SEND     0x02
#define QCOMM_POLL_ACCEPT   0x04
#define QCOMM_POLL_CONNECT  0x08

typedef enum tag_qcomm_move_method
{
    QCOMM_MOVE_FROM_BEGINNING, /* position the stream pointer relative to the beginning of the stream */
    QCOMM_MOVE_FROM_CURRENT,   /* position the stream pointer relative to the current position in the stream */
    QCOMM_MOVE_FROM_END        /* position the stream pointer relative to the end of the stream */
} t_move_method;

typedef enum tag_qcomm_boolean_property
{
      QCOMM_PROPERTY_IS_READ_ONLY   /* stream cannot be written but is read-only e.g. "file:myfile.txt?mode=r". Used by persistent streams. */
    , QCOMM_PROPERTY_IS_WRITE_ONLY  /* stream cannot be read but is write-only e.g. "file:myfile.txt?mode=w". Used by persistent streams. */
    , QCOMM_PROPERTY_IS_EXCLUSIVE   /* gain exclusive access to the channel. e.g. used with I2C to combine messages by sending/receiving without releasing the bus */
    , QCOMM_PROPERTY_NO_READ_AHEAD  /* do not read ahead on receives i.e., do not attempt to fill receive buffer. Only read requested number of bytes */
} t_qcomm_boolean_property;

typedef enum tag_qcomm_integer_property
{
    QCOMM_PROPERTY_BYTES_READ,
    QCOMM_PROPERTY_BYTES_WRITTEN
} t_qcomm_integer_property;

typedef enum tag_qcomm_double_property
{
    QCOMM_PROPERTY_BYTES_READ_PER_SECOND,
    QCOMM_PROPERTY_BYTES_WRITTEN_PER_SECOND
} t_qcomm_double_property;

typedef enum tag_qcomm_string_property
{
      QCOMM_PROPERTY_MANUFACTURER
    , QCOMM_PROPERTY_PRODUCT_NAME
    , QCOMM_PROPERTY_MODEL_NAME
    , QCOMM_PROPERTY_SERIAL_NUMBER
    , QCOMM_PROPERTY_PEER_ADDRESS   /* Peer's address. For UDP, it is the IP address of the current peer */
} t_qcomm_string_property;

/*
    Description:

    This function establishes a server connection which listens on the given URI.
    The URI specifies the protocol, address, port and options associated with
    the server connection. The Communications API uses the protocol to load a protocol-specific
    driver. For example:

        tcpip://localhost:17000             - listen on port 17000 using TCP/IP
        shmem://mymemory:1?memsize=8192     - listen via shared memory buffer. Use 8K buffers by default.
        pipe://mypipe:1?memsize=4096        - listen via a named pipe. Use 4K buffers for the pipe.

    Parameters:

    uri               = a URI indicating the connection on which to listen.
    non_blocking      = set to true (1) to prevent qcomm_accept calls from blocking.
    server_connection = a pointer to a t_connection variable in which the server connection
                        handle will be stored.

    Return value:

    Returns 0 on success. If an error occurs then a negative error code is returned.

    Error codes:

    -QERR_NON_BLOCKING_NOT_SUPPORTED    = non-blocking mode is not supported by the selected protocol
*/
EXTERN t_error
qcomm_listen(const char * uri, t_boolean non_blocking, t_connection * server_connection);

/*
    Description:

    This function connects to a listening connection referenced by the given URI.
    The URI specifies the protocol, address, port and options associated with
    the connection. The Communications API uses the protocol to load a protocol-specific
    driver. For example:

        tcpip://remotehost:17000            - connect to remotehost on port 17000 using TCP/IP
        shmem://mymemory:1?memsize=8192     - connect via an 8K shared memory buffer
        pipe://mypipe:1?memsize=4096        - connect via a 4K named pipe

    If the non_blocking flag is set to false (0), then this function will block
    until the connection is made.

    If the non_blocking flag is set to true (1), then this function will not block.
    If the connection cannot be completed immediately then -QERR_WOULD_BLOCK is
    returned. In this case, the connection may be completed using qcomm_poll with
    the QCOMM_POLL_CONNECT flag.

    Parameters:

    uri               = a URI indicating the listening connection to which to connect.
    non_blocking      = set to true (1) to make the client connection non-blocking.
    client_connection = a pointer to a t_connection variable in which the client connection
                        handle will be stored.

    Return value:

    Returns 0 on success. If an error occurs then a negative error code is returned.

    Error codes:

    QERR_TIMED_OUT = the timeout expired before the connection was made
*/
EXTERN t_error
qcomm_connect(const char * uri, t_boolean non_blocking, t_connection * client_connection);

/*
    Description:

    This function creates a connection to a buffer in memory rather than to a server.
    The ability to create a "memory channel" allows all the Quanser APIs based upon
    the Quanser Communications API to be used with in-memory buffers, including XML
    parsing, text formatting and conversions, etc.

    Memory channels never block so there is no need for a "non-blocking" argument.
    It is possible to both read and write to the buffer since both read and write operations 
    maintain a separate index. Normally, the buffer is treated as read-only (only qcomm_receive
    used) or write-only (only qcomm_send used).

    Parameters:

    buffer            = the address of the buffer to be used as the "memory channel".
    buffer_size       = the size of the buffer in bytes. The qcomm_receive function will
                        never read past this length. The qcomm_send function will never
                        write past this length.
    client_connection = a pointer to a t_connection variable in which the client connection
                        handle will be stored.

    Return value:

    Returns 0 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_error
qcomm_create_memory_connection(void * buffer, size_t buffer_size, t_connection * client_connection);

/*
    Description:

    This function accepts a connection to a listening communication channel by
    a client. The client connects using qcomm_connect.

    If qcomm_listen was called with non_blocking set to false (0) then this call will block
    until a client connects. The client connection returned will also be a blocking connection.

    If qcomm_listen was called with non_blocking set to true (1) then this call will not
    block. If there is no pending client connection then it will return -QERR_WOULD_BLOCK.
    The qcomm_poll function may be used with the QCOMM_POLL_ACCEPT flag to determine when
    a client connection is pending. In this case, the client connection returned will also
    be a non-blocking connection.

    On POSIX systems this function should be interruptible by a signal so that arrival
    of a signal will cause a -QERR_INTERRUPTED error to be returned.

    Parameters:

    server_connection  = a listening connection established using qcomm_listen.
    client_connection  = a pointer to a t_connection variable in which the client connection
                         handle will be stored.

    Return value:

    Returns 0 on success. If an error occurs then a negative error code is returned.

    Error codes:

    -QERR_WOULD_BLOCK   = the connection is in non-blocking mode and there are no pending 
                          client connections.
*/
EXTERN t_error
qcomm_accept(t_connection server_connection, t_connection * client_connection);

/*
    Description:

    This function polls the connection to determine whether it is possible to send or receive or
    accept a connection without blocking. The flags argument determines the conditions for which
    to check. The return value indicates the conditions which occurred. This function returns after
    the given timeout with a value of 0 if none of the conditions occurs. If an error occurs, then
    it returns a negative error code. The function will return before the timeout if at least
    one of the conditions occurs prior to the timeout.

    Parameters:

    connection  = a connection established using qcomm_listen, qcomm_connect or qcomm_accept.
    timeout     = a relative or absolute timeout which determines the maximum time that this
                  function will wait for one of the conditions to occur before returning. A
                  value of NULL indicates an infinite timeout.
    flags       = a bit mask indicating the conditions for which to check. Valid flags are:

                    QCOMM_POLL_RECEIVE  - on a listening connection, check for connections pending
                                          from clients. On a client connection, check whether there
                                          is any data available to receive.

                    QCOMM_POLL_SEND     - not valid on a listening connection. On a client connection,
                                          check whether there is space in the transmit buffer to
                                          send any data.

                    QCOMM_POLL_ACCEPT   - not valid on a client connection. On a listening connection,
                                          check whether there is a pending client connection.

                    QCOMM_POLL_CONNECT  - not valid on a listening connection. On a client connection,
                                          check whether the connection has completed.

    Return value:

    A bit mask containing the conditions which were satisfied. It has the same definition as the
    flags argument. If none of the specified conditions occurs before the timeout, then 0 is
    returned. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
qcomm_poll(t_connection connection, const t_timeout * timeout, t_uint flags);

/*
    Description:

    This function receives data over a client connection. It attempts to receive buffer_size bytes
    from the communication channel. 
    
    If qcomm_listen or qcomm_connect was called with the non-blocking flag set to false (0),
    then this function blocks until any data becomes available. If some data is available then it
    returns immediately with the number of bytes received. This may be less than buffer_size bytes.
    If the connection has been closed gracefully then it returns 0 only once there is no more data
    to receive. If an error occurs, then it returns a negative error code.

    If qcomm_listen or qcomm_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If no data is available then it returns -QERR_WOULD_BLOCK.
    In this case, the qcomm_poll function may be used with the QCOMM_POLL_RECEIVE flags to
    determine when data becomes available.

    This function does not support two threads calling qcomm_receive at the same time. However,
    qcomm_send may be called by another thread at the same time as qcomm_receive.

    The semantics of this function differ from the BSD recv() socket function in that it
    may not receive the amount of data expected, even in blocking mode. The semantics 
    differ to handle protocols, such as RT-TCP/IP, which cannot receive arbitrarily 
    large buffers in a single recv() operation. Multiple recv() operations are not 
    performed for RT-TCP/IP because it is not possible to distinguish whether multiple
    send() operations or a single send() operation was performed at the peer.
    By returning the number of bytes received from a single recv() operation, blocking
    is avoided when some bytes are available. Use the Stream API to receive the
    exact number of bytes requested.

    Parameters:

    connection  = a client connection established using qcomm_connect or qcomm_accept.
    buffer      = a buffer of at least buffer_size bytes in which the received data will be stored.
    buffer_size = the number of bytes available in the buffer.

    Return value:

    The number of bytes received, which may be less than buffer_size bytes. If no more data
    is available and the connection has been closed gracefully then 0 is returned. If an error
    occurs then a negative error code is returned.
*/
EXTERN t_int
qcomm_receive(t_connection connection, void * buffer, t_int buffer_size);

/*
    Description:

    This function sends data over a client connection. It attempts to send buffer_size bytes
    over the communication channel. If some space is available then it returns immediately
    with the number of bytes sent. This may be less than buffer_size bytes. If no space is
    available then its behavior depends on whether it was blocking are not. See the
    discussion below for details. If an error occurs, then it returns a negative error code.
    If the connection is closed it is considered an error condition.

    If qcomm_listen or qcomm_connect was called with the non-blocking flag set to false (0),
    then this function blocks until there is room in the connection buffer for any data. If 
    some room is available then it returns immediately with the number of bytes sent. This may
    be less than buffer_size bytes.  If an error occurs, then it returns a negative error code.
    If the connection is closed it is considered an error condition. Note that this behavior
    differs from the traditional send function used with sockets, which always sends all the
    data for a blocking socket unless the connection is closed.

    If qcomm_listen or qcomm_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If no room is available in the connection buffer, then
    it returns -QERR_WOULD_BLOCK. In this case, the qcomm_poll function may be used with
    the QCOMM_POLL_SEND flag to determine when space of at least one byte becomes available.

    This function does not support two threads calling qcomm_send at the same time. However,
    qcomm_send may be called by another thread at the same time as qcomm_receive.

    The semantics of this function differ from the BSD send() socket function in that it
    may not send all the data, even in blocking mode. The semantics differ to handle protocols,
    such as RT-TCP/IP, which cannot send arbitrarily large buffers in a single send()
    operation. Multiple send() operations are not performed for RT-TCP/IP because the
    corresponding recv() call at the peer may expect one message when it would,
    in fact, receive multiple messages if multiple send() operations were performed.
    By returning the number of bytes sent from a single send() operation, this situation
    is detectable. Use the Stream API if higher-level functionality is required.

    Parameters:

    connection  = a client connection established using qcomm_connect or qcomm_accept.
    buffer      = a buffer of at least buffer_size bytes containing the data to be sent.
    buffer_size = the number of bytes to send from the buffer.

    Return value:

    The number of bytes sent, which may be less than buffer_size bytes. If an error
    occurs then a negative error code is returned. A value of zero is only returned
    if the buffer_size is zero.
*/
EXTERN t_int
qcomm_send(t_connection connection, const void * buffer, t_int buffer_size);

/*
    Description:

    This function indicates whether the communication protocol supports the qcomm_send_and_receive
    function. It is also possible to call the qcomm_send_receive function with a buffer size of
    zero and see whether it returns -QERR_NOT_SUPPORTED or zero. However, this approach may send
    a zero-length datagram for those protocols which support it. The qcomm_has_send_and_receive
    function does not risk this possibility.

    Parameters:

    connection = a connection established using qcomm_listen, qcomm_connect or qcomm_accept.

    Return value:

    Returns true if the protocol supports qcomm_send_and_receive. Otherwise it
    returns false.
*/
EXTERN t_boolean
qcomm_has_send_and_receive(t_connection connection);

/*
    Description:

    This function both sends and receives data over a connection. It is an optional
    function that is typically only implemented for protocols such as SPI, which send
    and receive data with every transaction.

    Parameters:

    connection          = a connection established using qcomm_listen, qcomm_connect or qcomm_accept.
    send_buffer         = a buffer of at least buffer_size bytes containing the data to be sent.
    receive_buffer      = a buffer of at least buffer_size bytes to contain the data received.
    buffer_size         = the number of bytes to both send and receive.

    Return value:

    The number of bytes sent and received, which may be less than buffer_size bytes. If an error 
    occurs then a negative error code is returned. A value of zero is returned if the connection 
    has been closed gracefully.
*/
EXTERN t_int
qcomm_send_and_receive(t_connection connection, const void * send_buffer, void * receive_buffer, t_int buffer_size);

/*
    Description:

    This function flushes any remaining data in the driver's buffer to
    the underlying communication channel. It is typically used for filter
    drivers, since other drivers are generally unbuffered in the protocol
    driver (although the operating system may have internal buffering).

    This function is optional in a driver. If the driver does not provide
    it then this function returns 0 rather than -QERR_NOT_SUPPORTED because
    it is assumed that the driver has no internal buffering in this case
    and that there can be no remaining data (i.e. it is always "flushed").

    Parameters:

    connection  = a connection established using qcomm_listen, qcomm_connect or qcomm_accept.

    Return value:

    Returns 0 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_error
qcomm_flush(t_connection connection);

/*
 * Description:
 *
 * This function sets the current position of the underlying communication channel.
 * It flushes all data to the communication channel first so that no data is lost.
 * Any read buffers are also cleared so that subsequent reads begin at the new
 * position.
 *
 * The given position is relative to the beginning of the channel, current position
 * or end of the channel according to the method argument. See the t_move_method
 * enumeration for the appropriate constants.
 *
 * This function is optional in a driver. If the driver does not provide
 * it then this function returns -QERR_NOT_SUPPORTED.
 */
EXTERN t_long
qcomm_set_position(t_connection connection, t_long position, t_move_method method);

/*
 * Description:
 *
 * This function returns the current position of the underlying communication channel
 * relative to the beginning. It flushes all data to the communication channel first
 * so that the position reflects the current state of the channel.
 *
 * This function is optional in a driver. If the driver does not provide
 * it then this function returns -QERR_NOT_SUPPORTED.
 */
EXTERN t_long
qcomm_get_position(t_connection connection);

/*
 * Description:
 *
 * This function returns the value of the specified boolean properties.
 *
 * This function is optional in a driver. If the driver does not provide
 * it then this function returns -QERR_NOT_SUPPORTED.
 */
EXTERN t_error
qcomm_get_boolean_property(t_connection connection, const t_qcomm_boolean_property properties[], t_uint num_properties, t_boolean buffer[]);

/*
 * Description:
 *
 * This function sets the value of the specified boolean properties.
 *
 * This function is optional in a driver. If the driver does not provide
 * it then this function returns -QERR_NOT_SUPPORTED.
 */
EXTERN t_error
qcomm_set_boolean_property(t_connection connection, const t_qcomm_boolean_property properties[], t_uint num_properties, const t_boolean buffer[]);

/*
 * Description:
 *
 * This function returns the value of the specified integer properties.
 *
 * This function is optional in a driver. If the driver does not provide
 * it then this function returns -QERR_NOT_SUPPORTED.
 */
EXTERN t_error
qcomm_get_integer_property(t_connection connection, const t_qcomm_integer_property properties[], t_uint num_properties, t_int buffer[]);

/*
 * Description:
 *
 * This function sets the value of the specified integer properties.
 *
 * This function is optional in a driver. If the driver does not provide
 * it then this function returns -QERR_NOT_SUPPORTED.
 */
EXTERN t_error
qcomm_set_integer_property(t_connection connection, const t_qcomm_integer_property properties[], t_uint num_properties, const t_int buffer[]);

/*
 * Description:
 *
 * This function returns the value of the specified double properties.
 *
 * This function is optional in a driver. If the driver does not provide
 * it then this function returns -QERR_NOT_SUPPORTED.
 */
EXTERN t_error
qcomm_get_double_property(t_connection connection, const t_qcomm_double_property properties[], t_uint num_properties, t_double buffer[]);

/*
 * Description:
 *
 * This function sets the value of the specified double properties.
 *
 * This function is optional in a driver. If the driver does not provide
 * it then this function returns -QERR_NOT_SUPPORTED.
 */
EXTERN t_error
qcomm_set_double_property(t_connection connection, const t_qcomm_double_property properties[], t_uint num_properties, const t_double buffer[]);

/*
 * Description:
 *
 * This function returns the value of the specified string property.
 *
 * This function is optional in a driver. If the driver does not provide
 * it then this function returns -QERR_NOT_SUPPORTED.
 */
EXTERN t_error
qcomm_get_string_property(t_connection connection, const t_qcomm_string_property property_code, char * buffer, size_t buffer_size);

/*
 * Description:
 *
 * This function sets the value of the specified string property.
 *
 * This function is optional in a driver. If the driver does not provide
 * it then this function returns -QERR_NOT_SUPPORTED.
 */
EXTERN t_error
qcomm_set_string_property(t_connection connection, const t_qcomm_string_property property_code, const char * buffer, size_t buffer_size);

/*
    Description:

    This function shuts down a connection. It interrupts blocking
    send operations so that they return with -QERR_CONNECTION_SHUTDOWN.
    It should also eventually interrupt receive operations. For TCP/IP
    connections, receive operations may continue to gather any remaining
    bytes sent from the peer before indicating that the connection has
    been closed by the peer.

    Parameters:

    connection  = a connection established using qcomm_listen, qcomm_connect or qcomm_accept.

    Return value:

    Returns 0 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_error
qcomm_shutdown(t_connection connection);

/*
    Description:

    This function closes the connection. All resources associated with
    the connection are freed.

    Parameters:

    connection  = a connection established using qcomm_listen, qcomm_connect or qcomm_accept.

    Return value:

    Returns 0 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_error
qcomm_close(t_connection connection);

#endif
