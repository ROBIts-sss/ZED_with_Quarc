/*
    The Quanser Stream API is a generic buffered communication
    protocol API based upon the Quanser Communications API defined
    in quanser_communications.h.
*/
#if !defined(_quanser_stream_h)
#define _quanser_stream_h

#include <stdarg.h>
#include "ptrarg.h"
#include "quanser_communications.h"

typedef struct tag_stream * t_stream;

typedef struct tag_stream_poke_state
{
    t_int bytes_poked;  /* number of bytes poked so far */
} t_stream_poke_state;

typedef struct tag_stream_peek_state
{
    t_int bytes_peeked; /* number of bytes peeked so far */
} t_stream_peek_state;

/* maximum stream buffer size */
#define MAX_STREAM_BUFFER_SIZE      ((1 << 30) - 1)

/* Flags used by the stream_poll function */
#define STREAM_POLL_RECEIVE  QCOMM_POLL_RECEIVE
#define STREAM_POLL_SEND     QCOMM_POLL_SEND
#define STREAM_POLL_ACCEPT   QCOMM_POLL_ACCEPT
#define STREAM_POLL_CONNECT  QCOMM_POLL_CONNECT
#define STREAM_POLL_FLUSH    0x80

/* Flags used by the stream_set_position method */
#define STREAM_MOVE_FROM_BEGINNING  QCOMM_MOVE_FROM_BEGINNING
#define STREAM_MOVE_FROM_CURRENT    QCOMM_MOVE_FROM_CURRENT
#define STREAM_MOVE_FROM_END        QCOMM_MOVE_FROM_END

/* Flags used by the stream_set_byte_order method */
typedef enum tag_stream_byte_order
{
    STREAM_BYTE_ORDER_NATIVE_ENDIAN,
    STREAM_BYTE_ORDER_LITTLE_ENDIAN,
    STREAM_BYTE_ORDER_BIG_ENDIAN
} t_stream_byte_order;

typedef enum tag_stream_boolean_property
{
      STREAM_PROPERTY_IS_READ_ONLY  = QCOMM_PROPERTY_IS_READ_ONLY   /* stream cannot be written but is read-only e.g. "file:myfile.txt?mode=r". Used by persistent streams. */
    , STREAM_PROPERTY_IS_WRITE_ONLY = QCOMM_PROPERTY_IS_WRITE_ONLY  /* stream cannot be read but is write-only e.g. "file:myfile.txt?mode=w". Used by persistent streams. */
    , STREAM_PROPERTY_IS_EXCLUSIVE  = QCOMM_PROPERTY_IS_EXCLUSIVE   /* gain exclusive access to the stream. e.g. used with I2C to combine messages by sending/receiving without releasing the bus */
    , STREAM_PROPERTY_NO_READ_AHEAD = QCOMM_PROPERTY_NO_READ_AHEAD  /* do not read ahead on receives i.e., do not attempt to fill receive buffer. Only read requested number of bytes */
} t_stream_boolean_property;

typedef enum tag_stream_integer_property
{
    STREAM_PROPERTY_BYTES_READ    = QCOMM_PROPERTY_BYTES_READ,
    STREAM_PROPERTY_BYTES_WRITTEN = QCOMM_PROPERTY_BYTES_WRITTEN
} t_stream_integer_property;

typedef enum tag_stream_double_property
{
    STREAM_PROPERTY_BYTES_READ_PER_SECOND    = QCOMM_PROPERTY_BYTES_READ_PER_SECOND,
    STREAM_PROPERTY_BYTES_WRITTEN_PER_SECOND = QCOMM_PROPERTY_BYTES_WRITTEN_PER_SECOND
} t_stream_double_property;

typedef enum tag_stream_string_property
{
      STREAM_PROPERTY_MANUFACTURER  = QCOMM_PROPERTY_MANUFACTURER
    , STREAM_PROPERTY_PRODUCT_NAME  = QCOMM_PROPERTY_PRODUCT_NAME
    , STREAM_PROPERTY_MODEL_NAME    = QCOMM_PROPERTY_MODEL_NAME
    , STREAM_PROPERTY_SERIAL_NUMBER = QCOMM_PROPERTY_SERIAL_NUMBER
    , STREAM_PROPERTY_PEER_ADDRESS  = QCOMM_PROPERTY_PEER_ADDRESS   /* Peer's address. For UDP, it is the IP address of the current peer */
} t_stream_string_property;

/* Character formats supported by the Stream API */
typedef enum tag_stream_format
{
    STREAM_FORMAT_AUTO,         /* determine character format automatically (requires BOM for UTF-16 and UTF-32) */
    STREAM_FORMAT_UTF8,         /* UTF-8 characters (one byte code unit, up to 4 code units per character) */
    STREAM_FORMAT_UTF16,        /* UTF-16 characters (two byte code unit, up to 2 code units per character) */
    STREAM_FORMAT_UTF32,        /* UTF-32 characters (four byte code unit, always one code unit per character) */

    NUM_STREAM_FORMATS
} t_stream_format;

#define stream_receive_char(stream, buffer, max_code_units) \
    stream_receive_utf8_char(stream, buffer, max_code_units)
#define stream_send_char(stream, buffer, max_code_units) \
    stream_send_utf8_char(stream, buffer, max_code_units)

#if WCHAR_MAX < 0x0100

#define stream_receive_wchar(stream, buffer, max_code_units) \
    stream_receive_utf8_char(stream, buffer, max_code_units)
#define stream_send_wchar(stream, buffer, max_code_units) \
    stream_send_utf8_char(stream, buffer, max_code_units)

#elif WCHAR_MAX < 0x010000

#define stream_receive_wchar(stream, buffer, max_code_units) \
    stream_receive_utf16_char(stream, buffer, max_code_units)
#define stream_send_wchar(stream, buffer, max_code_units) \
    stream_send_utf16_char(stream, buffer, max_code_units)

#else

#define stream_receive_wchar(stream, buffer, max_code_units) \
    stream_receive_utf32_char(stream, buffer)
#define stream_send_wchar(stream, buffer, max_code_units) \
    stream_send_utf32_char(stream, buffer[0])

#endif

/*
    Name:   stream_listen

    Description:

    This function establishes a server stream which listens on the given URI.
    The URI specifies the protocol, address, port and options associated with
    the server stream. The Stream API uses the protocol to load a protocol-specific
    driver. For example:

        tcpip://localhost:17000             - listen on port 17000 using TCP/IP
        shmem://mymemory:1?bufsize=8192     - listen via shared memory buffer. Use 8K buffers by default.
        pipe:mypipe?bufsize=4096            - listen via a named pipe. Use 4K buffers for the pipe.

    Parameters:

    uri           = a URI indicating the stream on which to listen.
    non_blocking  = set to true (1) to prevent stream_accept calls from blocking.
    server_stream = a pointer to a t_stream variable in which the server stream
                    handle will be stored.

    Return value:

    Returns 0 on success. If an error occurs then a negative error code is returned.

    Error codes:

    -QERR_NON_BLOCKING_NOT_SUPPORTED    = non-blocking mode is not supported by the selected protocol
*/
EXTERN t_error
stream_listen(const char * uri, t_boolean non_blocking, t_stream * server_stream);

/*
    Name:   stream_connect

    Description:

    This function connects to a listening stream referenced by the given URI.
    The URI specifies the protocol, address, port and options associated with
    the stream. The Stream API uses the protocol to load a protocol-specific
    driver. For example:

        tcpip://remotehost:17000            - connect to remotehost on port 17000 using TCP/IP
        shmem://mymemory:1?bufsize=8192     - connect via an 8K shared memory buffer
        pipe:mypipe?bufsize=4096            - connect via a 4K named pipe

    If the non_blocking flag is set to false (0), then this function will block
    until the connection is made.

    If the non_blocking flag is set to true (1), then this function will not block.
    If the connection cannot be completed immediately then -QERR_WOULD_BLOCK is
    returned. In this case, the connection may be completed using stream_poll with
    the STREAM_POLL_CONNECT flag.

    Parameters:

    uri                 = a URI indicating the listening stream to which to connect.
    non_blocking        = set to true (1) to make the client connection non-blocking.
    send_buffer_size    = the size of the buffer to use for sending data over the stream, in bytes
    receive_buffer_size = the size of the buffer to use for receiving data over the stream, in bytes
    client_stream       = a pointer to a t_stream variable in which the client stream
                          handle will be stored.

    Return value:

    Returns 0 on success. If an error occurs then a negative error code is returned.

    Error codes:

    -QERR_WOULD_BLOCK = the connection request would block.
*/
EXTERN t_error
stream_connect(const char * uri, t_boolean non_blocking, t_int send_buffer_size, t_int receive_buffer_size, 
               t_stream * client_stream);

/*
    Name:   stream_create_memory_stream

    Description:

    This function create a "memory stream" that may be used to read from or write to
    a buffer in memory instead of a communication channel. Send and receive buffers are
    still used for parsing the buffer contents and so they should be treated as they
    would be for any other communication channel.

    Memory streams never block so there is no non-blocking argument.

    Parameters:

    memory              = the memory buffer from which to read or to which to write
    memory_size         = the size of the memory buffer invites. The stream_receive functions
                          will never read past this length. The stream_send functions will
                          never write past this length.
    send_buffer_size    = the size of the buffer to use for sending data over the stream, in bytes
    receive_buffer_size = the size of the buffer to use for receiving data over the stream, in bytes
    client_stream       = a pointer to a t_stream variable in which the client stream
                          handle will be stored.

    Return value:

    Returns 0 on success. If an error occurs then a negative error code is returned.

    Error codes:

    -QERR_WOULD_BLOCK = the connection request would block.
*/
EXTERN t_error
stream_create_memory_stream(void * memory, size_t memory_size, t_int send_buffer_size, t_int receive_buffer_size,
                            t_stream * client_stream);

/*
    Name:   stream_accept

    Description:

    This function accepts a connection to a listening communication stream by
    a client. The client connects using stream_connect.

    If stream_listen was called with non_blocking set to false (0) then this call will block
    until a client connects. The client stream returned will also be a blocking stream.

    If stream_listen was called with non_blocking set to true (1) then this call will not
    block. If there is no pending client connection then it will return -QERR_WOULD_BLOCK.
    The stream_poll function may be used with the STREAM_POLL_ACCEPT flag to determine when
    a client connection is pending. In this case, the client stream returned will also 
    be a non-blocking stream.

    On POSIX systems this function should be interruptible by a signal so that arrival
    of a signal will cause a -QERR_INTERRUPTED error to be returned.

    Parameters:

    server_stream       = a listening stream established using stream_listen.
    send_buffer_size    = the size of the buffer to use for sending data over the stream, in bytes
    receive_buffer_size = the size of the buffer to use for receiving data over the stream, in bytes
    client_stream       = a pointer to a t_stream variable in which the client stream
                          handle will be stored.

    Return value:

    Returns 0 on success. If an error occurs then a negative error code is returned.

    Error codes:

    -QERR_WOULD_BLOCK   = the connection is in non-blocking mode and there are no pending 
                          client connections.
*/
EXTERN t_error
stream_accept(t_stream server_stream, t_int send_buffer_size, t_int receive_buffer_size, t_stream * client_stream);

/*
    Name:   stream_set_swap_bytes

    Description:

    This function determines whether the functions that send and receive t_short's,
    t_utf16_char's, t_utf32_char's, t_int's, t_long's, or arrays of them, swap the 
    bytes in the individual values.

    Byte swapping is only necessary when communicating between processors that use
    a different endianness.  For example, Intel processors are little endian (LSB first)
    while Motorola processors tend to be big endian (MSB first). By default, no
    byte swapping takes place. Whether byte swapping is necessary may be determined
    simply by sending 0x1234 as a short and seeing if it arrives as the same number
    or 0x3421.

    Note that stream_set_swap_bytes and stream_set_byte_order override each other.
    The function most recently called will determine whether byte swapping occurs.

    An error is only returned if the stream is invalid.

    Parameters:

    stream  = a stream established using stream_listen, stream_connect or stream_accept.
    swap    = non-zero to enable byte swapping, and zero to disable byte swapping.

    Return value:

    Returns the previous byte swapping state on success or a negative error code
    if an error occurs. A return value of 0 indicates that byte swapping was not
    enabled prior to the call, and a return value of 1 indicates that byte swapping
    was enabled prior to the call.
*/
EXTERN t_error
stream_set_swap_bytes(t_stream stream, t_boolean swap);

/*
    Name:   stream_set_byte_order

    Description:

    This function determines whether the functions that send and receive t_short's,
    t_utf16_char's, t_utf32_char's, t_int's, t_long's, or arrays of them, swap the 
    bytes in the individual values.

    Byte swapping is only necessary when communicating between processors that use
    a different endianness. For example, Intel processors are little endian (LSB first)
    while Motorola processors tend to be big endian (MSB first). By default, no
    byte swapping takes place. This function compares the byte order given to the
    native byte ordering of the platform running the code and tells the stream to
    swap bytes if the byte orders are different.

    Note that stream_set_swap_bytes and stream_set_byte_order override each other.
    The function most recently called will determine whether byte swapping occurs.

    Parameters:

    stream      = a stream established using stream_listen, stream_connect or stream_accept.
    byte_order  = the desired byte order when sending/receiving data types. Pass
                  STREAM_BYTE_ORDER_NATIVE_ENDIAN to disable byte swapping and use the
                  native byte ordering of the platform (the default).

    Return value:

    Returns 0 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_error
stream_set_byte_order(t_stream stream, t_stream_byte_order byte_order);

/*
    Name:   stream_set_character_format

    Description:

    This function determines whether the functions that send and receive t_utf8_char's,
    t_utf16_char's or t_utf32_char's, or arrays of them, convert between another character
    format when writing to or reading from the underlying communication channel.
    
    Character format conversions are only necessary when communicating between platforms
    that use a different character format. For example, Unix systems may use UTF-32 wide
    characters while Windows uses UTF-16 wide characters. By default, the character
    format is assumed to be the same as the local character format.

    Parameters:

    stream  = a stream established using stream_listen, stream_connect or stream_accept.
    format  = the character format of the peer

    Return value:

    Returns 0 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_error
stream_set_character_format(t_stream stream, t_stream_format format);

/*
    Name:   stream_get_character_format

    Description:

    This function returns the character format of the stream. If the character format
    has not yet been determined then STREAM_FORMAT_AUTO is returned. Otherwise the
    character format detected or set manually is returned.
    
    Character format conversions are only necessary when communicating between platforms
    that use a different character format. For example, Unix systems may use UTF-32 wide
    characters while Windows uses UTF-16 wide characters. By default, the character
    format is assumed to be the same as the local character format.

    Parameters:

    stream  = a stream established using stream_listen, stream_connect or stream_accept.
    format  = the character format of the peer

    Return value:

    Returns 0 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_error
stream_get_character_format(t_stream stream, t_stream_format * format);

/*
    Name:   stream_poll

    Description:

    This function polls the stream to determine whether it is possible to send or receive or
    accept a connection without blocking. The flags argument determines the conditions for which
    to check. The return value indicates the conditions which occurred. This function returns after
    the given timeout with a value of 0 if none of the conditions occurs. If an error occurs, then
    it returns a negative error code. The function will return before the timeout if at least
    one of the conditions occurs prior to the timeout.

    Parameters:

    stream  = a stream established using stream_listen, stream_connect or stream_accept.
    timeout = a relative or absolute timeout which determines the maximum time that this
              function will wait for one of the conditions to occur before returning. A
              value of NULL indicates an infinite timeout.
    flags   = a bit mask indicating the conditions for which to check. Valid flags are:

                STREAM_POLL_RECEIVE  - on a listening stream, check for connections pending
                                       from clients. On a client stream, check whether there
                                       is any data available to receive.

                STREAM_POLL_SEND     - not valid on a listening stream. On a client stream,
                                       check whether there is space in the stream buffer to
                                       store any data.

                STREAM_POLL_FLUSH    - not valid on a listening stream. On a client stream,
                                       check whether it is possible to flush any more data
                                       without blocking.

                STREAM_POLL_ACCEPT   - not valid on a client stream. On a listening stream,
                                       check whether there is a pending client connection.

                STREAM_POLL_CONNECT  - not valid on a listening stream. On a client stream,
                                       check whether the connection has completed.

    Return value:

    A bit mask containing the conditions which were satisfied. It has the same definition as the
    flags argument. If none of the specified conditions occurs before the timeout, then 0 is
    returned. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_poll(t_stream stream, const t_timeout * timeout, t_uint flags);

/*
    Name:   stream_ignore

    Description:

    This function receives data over a client stream. However this data is simply
    discarded. It is not returned to the caller. It attempts to receive length bytes
    from the communication channel. 
    
    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read. If the connection has been closed
    gracefully then it returns 0 only once there is no more data to receive. Otherwise it
    returns the number of bytes read before the connection closed. Once all the data in the
    stream buffer is exhausted it will return 0 to indicate the connection has been closed.
    If an error occurs, then it returns a negative error code.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If no data is available at all then it returns
    -QERR_WOULD_BLOCK. In this case, the stream_poll function may be used with the
    STREAM_POLL_RECEIVE flag to determine when data becomes available. Otherwise it returns
    the number of bytes received.

    This function does not support two threads calling stream_receive at the same time. However,
    stream_send or stream_flush may be called by another thread at the same time as stream_receive.

    The semantics of this function differ from the BSD recv() socket function because it
    receives length bytes in blocking mode rather than the number of bytes that were
    sent in a single send() call at the peer. The semantics differ because this function
    attempts to "read ahead" by keeping the stream buffer full, thereby minimizing the
    number of receive operations performed on the internal connection. Also, due to
    buffering of the stream_send operation, the number of send() calls made at the
    peer may not correspond to the number expected.

    Parameters:

    stream = a client stream established using stream_connect or stream_accept.
    length = the number of bytes to ignore.

    Return value:

    The number of bytes received, which may be less than length bytes for non-blocking streams.
    If no more data is available and the connection has been closed gracefully then 0 is returned. 
    If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_ignore(t_stream stream, t_int length);

/*
    Name:   stream_receive

    Description:

    This function receives data over a client stream. It attempts to receive buffer_size bytes
    from the communication channel. 
    
    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read. If the connection has been closed
    gracefully then it returns 0 only once there is no more data to receive. Otherwise it
    returns the number of bytes read before the connection closed. Once all the data in the
    stream buffer is exhausted it will return 0 to indicate the connection has been closed.
    If an error occurs, then it returns a negative error code.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If no data is available at all then it returns
    -QERR_WOULD_BLOCK. In this case, the stream_poll function may be used with the
    STREAM_POLL_RECEIVE flag to determine when data becomes available. Otherwise it returns
    the number of bytes received.

    Unlike the stream_receive_byte_array function, this function does not require that the
    stream receive buffer be at least buffer_size bytes in length. Hence, it allow smaller
    stream buffers to be used.

    This function does not support two threads calling stream_receive at the same time. However,
    stream_send or stream_flush may be called by another thread at the same time as stream_receive.

    The semantics of this function differ from the BSD recv() socket function because it
    receives buffer_size bytes in blocking mode rather than the number of bytes that were
    sent in a single send() call at the peer. The semantics differ because this function
    attempts to "read ahead" by keeping the stream buffer full, thereby minimizing the
    number of receive operations performed on the internal connection. Also, due to
    buffering of the stream_send operation, the number of send() calls made at the
    peer may not correspond to the number expected.

    Parameters:

    stream      = a client stream established using stream_connect or stream_accept.
    buffer      = a buffer of at least buffer_size bytes in which the received data will be stored.
    buffer_size = the number of bytes available in the buffer.

    Return value:

    The number of bytes received, which may be less than buffer_size bytes for non-blocking streams.
    If no more data is available and the connection has been closed gracefully then 0 is returned. 
    If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_receive(t_stream stream, void * buffer, t_int buffer_size);

/*
    Name:   stream_receive_byte

    Description:

    This function receives a single byte over a client stream. It has the same
    semantics as the stream_receive function except that it only receives a
    single byte.

    Parameters:

    stream      = a client stream established using stream_connect or stream_accept.
    value       = a pointer to a t_byte variable in which the received data will be stored.

    Return value:

    The number of bytes received, which will always be 1. If no more data is
    available and the connection has been closed gracefully then 0 is returned. 
    If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_receive_byte(t_stream stream, t_byte * value);

/*
    Name:   stream_receive_bytes

    Description:

    This function receives an array of bytes over a client stream. It has the same
    semantics as the stream_receive function.

    Unlike the stream_receive_byte_array function, this function does not require that the
    stream receive buffer be at least num_elements bytes in length. Hence, it allow smaller
    stream buffers to be used.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = a pointer to a t_byte array in which the received data will be stored.
    num_elements = the number of elements to receive, and available in the elements array.

    Return value:

    The number of bytes received, which may be less than the requested number of bytes
    for non-blocking streams. If no more data is available and the connection has been
    closed gracefully then 0 is returned. If an error occurs then a negative error code
    is returned.
*/
EXTERN t_int
stream_receive_bytes(t_stream stream, t_byte * elements, t_uint num_elements);

/*
    Name:   stream_receive_byte_array

    Description:

    This function receives an array of bytes over a client stream. It differs from
    the stream_receive_bytes function in that it treats the entire array as an
    atomic unit. It either receives all of the array or none of it. It also requires
    that the stream receive buffer be at least as large as the array of bytes.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    the entire array then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of the entire array. Otherwise it returns 1. Once
    there are fewer bytes left to receive than the size of the entire array then it will
    return 0 to indicate the connection has been closed. Use stream_receive to receive any
    remaining bytes if required. If an error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = a pointer to a t_byte array in which the received data will be stored.
    num_elements = the number of elements to receive, and available in the elements array.

    Return value:

    Returns 1 on success. If not enough data is available and the connection has been
    closed gracefully then 0 is returned. If an error occurs then a negative error code
    is returned.
*/
EXTERN t_int
stream_receive_byte_array(t_stream stream, t_byte * elements, t_uint num_elements);

/*
    Name:   stream_receive_utf8_char

    Description:

    This function receives a full UTF-8 character over a client stream. Since UTF-8
    characters are variable-sized and may be up to four code units (bytes) long, this
    function may return between 1 and 4 bytes, extracting a complete UTF-8 character
    from the input stream. The return value is the number of code units (bytes) stored
    in the buffer.

    If the buffer is too small for the UTF-8 character (only possible if max_code_units
    is less than four) then -QERR_BUFFER_TOO_SMALL is returned and nothing is read from
    the input stream. Hence, a subsequent call with a larger buffer can succeed in
    reading the UTF-8 character.

    If an illegal UTF-8 character is encountered then -QERR_ILLEGAL_UTF8_CHARACTER is
    returned. If the lead byte was valid then the number of bytes indicated by the lead
    byte (1-4) are removed from the input stream. If the lead byte was not valid then
    only the invalid lead byte is removed from the input stream.

    If an illegal UTF-16 character is encountered then -QERR_ILLEGAL_UTF16_CHARACTER is
    returned. If the lead code unit was valid then the number of words indicated by the lead
    code unit (1-2) are removed from the input stream. If the lead code unit was not valid then
    only the invalid lead code unit is removed from the input stream.

    If an illegal UTF-32 character is encountered then -QERR_ILLEGAL_UTF32_CHARACTER is
    returned. The invalid character is removed from the input stream.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then 
    this function will swap the order of the bytes that it receives before storing them
    in the given buffer.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    a character then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns the number of code units returned in the caller's buffer.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of a character. Otherwise it returns the number
    of characters read before the connection closed. Once there are fewer bytes left to
    receive than the size of a character then it will return 0 to indicate the connection
    has been closed. Use stream_receive to receive any remaining bytes if required. If an
    error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    buffer         = a pointer to an array of t_utf8_char's in which the received character
                     will be stored. This buffer should contain at least 4 elements to
                     ensure that a full UTF-8 character is received (UTF-8 characters may
                     be up to 4 code units or bytes in length).
    max_code_units = the size of the buffer in code units (bytes)

    Return value:

    The number of code units written to the caller's buffer. If fewer bytes than the size
    of a character are available and the connection has been closed gracefully then 0 is
    returned. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_receive_utf8_char(t_stream stream, t_utf8_char * buffer, t_int max_code_units);

/*
    Name:   stream_receive_utf8_chars

    Description:

    This function receives an array of UTF-8 characters over a client stream.
    
    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of a
    UTF-8 character then it returns -QERR_WOULD_BLOCK. In this regard, it differs from
    the stream_receive_utf8_char_array function which returns -QERR_WOULD_BLOCK if the
    entire array cannot be returned without blocking.
    
    When -QERR_WOULD_BLOCK is returned, the stream_poll function may be used with the
    STREAM_POLL_RECEIVE flag to determine when data becomes available. Otherwise this
    functions returns the number of code units returned in the caller's buffer.
    This number may be smaller than the number requested even if all the requested bytes
    are available because this function never reads a partial character. UTF-8 characters
    are variable-sized and up to four code units (bytes) long. Hence, there may be 1-3
    code units (bytes) that cannot be stored in the supplied buffer because the next
    character in the input stream is too large to fit in the remaining bytes.

    If stream_listen or stream_connect was called with the non-blocking flag set to
    false (0), then this function attempts to return the number of code units requested
    unless the stream is closed by the peer before the full quantity is received.
    
    The return value is the number of code units (bytes) stored in the supplied buffer.

    If the buffer is too small for a UTF-8 character (only possible if max_code_units
    is less than four) then -QERR_BUFFER_TOO_SMALL is returned and nothing is read from
    the input stream. Hence, a subsequent call with a larger buffer can succeed in
    reading the UTF-8 character.

    If an illegal UTF-8 character is encountered then -QERR_ILLEGAL_UTF8_CHARACTER is
    returned. If the lead byte was valid then the number of bytes indicated by the lead
    byte (1-4) are removed from the input stream. If the lead byte was not valid then
    only the invalid lead byte is removed from the input stream.

    If an illegal UTF-16 character is encountered then -QERR_ILLEGAL_UTF16_CHARACTER is
    returned. If the lead code unit was valid then the number of words indicated by the lead
    code unit (1-2) are removed from the input stream. If the lead code unit was not valid then
    only the invalid lead code unit is removed from the input stream.

    If an illegal UTF-32 character is encountered then -QERR_ILLEGAL_UTF32_CHARACTER is
    returned. The invalid character is removed from the input stream.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then 
    this function will swap the order of the bytes that it receives before storing them
    in the given buffer.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of a character. Otherwise it returns the number
    of code units read before the connection closed. Once there are fewer bytes left to
    receive than the size of a character then it will return 0 to indicate the connection
    has been closed. Use stream_receive to receive any remaining bytes if required. If an
    error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    buffer         = a pointer to an array of t_utf8_char's in which the received characters
                     will be stored. This buffer should contain at least 4 elements to
                     ensure that a full UTF-8 character is received (UTF-8 characters may
                     be up to 4 code units or bytes in length).
    max_code_units = the size of the buffer in code units (bytes)

    Return value:

    The number of code units written to the caller's buffer. If fewer bytes than the size
    of a character are available and the connection has been closed gracefully then 0 is
    returned. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_receive_utf8_chars(t_stream stream, t_utf8_char * buffer, t_int max_code_units);

/*
    Name:   stream_receive_utf8_char_array

    Description:

    This function receives an array of UTF-8 characters over a client stream. It returns
    exactly the number of code units requested. Since UTF-8 characters are variable-sized 
    and may be up to four code units (bytes) long, this function may return an error if the
    characters received do not fit exactly within the number of code units requested, since
    it only extracts complete UTF-8 characters from the input stream. The return value is 1
    if the entire array is received. It returns a negative error code if an error occurs.
    If the peer closes the stream gracefully then zero is returned.

    If the buffer is too small for a UTF-8 character (only possible if max_code_units
    is less than four) then -QERR_BUFFER_TOO_SMALL is returned and nothing is read from
    the input stream. Hence, a subsequent call with a larger buffer can succeed in
    reading the UTF-8 character.

    If an illegal UTF-8 character is encountered then -QERR_ILLEGAL_UTF8_CHARACTER is
    returned.

    If an illegal UTF-16 character is encountered then -QERR_ILLEGAL_UTF16_CHARACTER is
    returned.

    If an illegal UTF-32 character is encountered then -QERR_ILLEGAL_UTF32_CHARACTER is
    returned.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then 
    this function will swap the order of the bytes that it receives before storing them
    in the given buffer.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the number of
    code units specified then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns the number of code units returned in the caller's buffer.
    This number may be smaller than the number requested even if all the requested bytes
    are available because this function never reads a partial character. Thus, there may
    be 1-3 bytes unused at the end of the buffer if the next character would occupy more
    than the bytes available.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of a character. Otherwise it returns the number
    of characters read before the connection closed. Once there are fewer bytes left to
    receive than the size of a character then it will return 0 to indicate the connection
    has been closed. Use stream_receive to receive any remaining bytes if required. If an
    error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    buffer         = a pointer to an array of t_utf8_char's in which the received characters
                     will be stored.
    max_code_units = the size of the buffer in code units (bytes)

    Return value:

    Returns 1 upon success. If fewer code units than the size of the array are available and the 
    connection has been closed gracefully then 0 is returned. If an error occurs then a 
    negative error code is returned.
*/
EXTERN t_int
stream_receive_utf8_char_array(t_stream stream, t_utf8_char * buffer, t_int max_code_units);

/*
    Name:   stream_receive_utf16_char

    Description:

    This function receives a full UTF-16 character over a client stream. Since UTF-16
    characters are variable-sized and may be up to two code units (words) long, this
    function may return between 1 or 2 words, extracting a complete UTF-16 character
    from the input stream. The return value is the number of code units (words) stored
    in the buffer.

    If the buffer is too small for the UTF-16 character (possible if last character
    does not fit in buffer) then -QERR_BUFFER_TOO_SMALL is returned and nothing is read from
    the input stream. Hence, a subsequent call with a larger buffer can succeed in
    reading the UTF-16 character.

    If an illegal UTF-8 character is encountered then -QERR_ILLEGAL_UTF8_CHARACTER is
    returned. If the lead byte was valid then the number of bytes indicated by the lead
    byte (1-4) are removed from the input stream. If the lead byte was not valid then
    only the invalid lead byte is removed from the input stream.

    If an illegal UTF-16 character is encountered then -QERR_ILLEGAL_UTF16_CHARACTER is
    returned. If the lead code unit was valid then the number of words indicated by the lead
    code unit (1-2) are removed from the input stream. If the lead code unit was not valid then
    only the invalid lead code unit is removed from the input stream.

    If an illegal UTF-32 character is encountered then -QERR_ILLEGAL_UTF32_CHARACTER is
    returned. The invalid character is removed from the input stream.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then 
    this function will swap the order of the bytes that it receives before storing them
    in the given buffer. Note that stream_set_character_format changes the swap bytes setting
    of the stream if the STREAM_FORMAT_AUTO format is used and it detects a different byte
    order in the UTF byte-order marker (BOM).

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    a character then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns the number of characters received.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of a character. Otherwise it returns the number
    of characters read before the connection closed. Once there are fewer bytes left to
    receive than the size of a character then it will return 0 to indicate the connection
    has been closed. Use stream_receive to receive any remaining bytes if required. If an
    error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    buffer         = a pointer to an array of t_utf16_char's in which the received character
                     will be stored. This buffer should contain at least 2 elements to
                     ensure that a full UTF-16 character is received (UTF-16 characters may
                     be up to 2 code units or words in length).
    max_code_units = the size of the buffer in code units (bytes)

    Return value:

    The number of characters received, which may be less than the requested number 
    for non-blocking streams. If fewer bytes than the size of a character are available
    and the connection has been closed gracefully then 0 is returned. If an error occurs
    then a negative error code is returned.
*/
EXTERN t_int
stream_receive_utf16_char(t_stream stream, t_utf16_char * buffer, t_int max_code_units);

/*
    Name:   stream_receive_utf16_chars

    Description:

    This function receives an array of UTF-16 characters over a client stream.
    
    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of a
    UTF-16 character then it returns -QERR_WOULD_BLOCK. In this regard, it differs from
    the stream_receive_utf16_char_array function which returns -QERR_WOULD_BLOCK if the
    entire array cannot be returned without blocking.
    
    When -QERR_WOULD_BLOCK is returned, the stream_poll function may be used with the
    STREAM_POLL_RECEIVE flag to determine when data becomes available. Otherwise this
    functions returns the number of code units returned in the caller's buffer.
    This number may be smaller than the number requested even if all the requested bytes
    are available because this function never reads a partial character. UTF-16 characters
    are variable-sized and up to two code units (words) long. Hence, there may be one
    code unit (word) that cannot be stored in the supplied buffer because the next
    character in the input stream is too large to fit in the remaining bytes.

    If stream_listen or stream_connect was called with the non-blocking flag set to
    false (0), then this function attempts to return the number of code units requested
    unless the stream is closed by the peer before the full quantity is received.
    
    The return value is the number of code units (words) stored in the supplied buffer.

    If the buffer is too small for a UTF-16 character (only possible if max_code_units
    is less than two) then -QERR_BUFFER_TOO_SMALL is returned and nothing is read from
    the input stream. Hence, a subsequent call with a larger buffer can succeed in
    reading the UTF-16 character.

    If an illegal UTF-8 character is encountered then -QERR_ILLEGAL_UTF8_CHARACTER is
    returned. If the lead byte was valid then the number of bytes indicated by the lead
    byte (1-4) are removed from the input stream. If the lead byte was not valid then
    only the invalid lead byte is removed from the input stream.

    If an illegal UTF-16 character is encountered then -QERR_ILLEGAL_UTF16_CHARACTER is
    returned. If the lead code unit was valid then the number of words indicated by the lead
    code unit (1-2) are removed from the input stream. If the lead code unit was not valid then
    only the invalid lead code unit is removed from the input stream.

    If an illegal UTF-32 character is encountered then -QERR_ILLEGAL_UTF32_CHARACTER is
    returned. The invalid character is removed from the input stream.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then 
    this function will swap the order of the bytes that it receives before storing them
    in the given buffer.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of a character. Otherwise it returns the number
    of code units read before the connection closed. Once there are fewer bytes left to
    receive than the size of a character then it will return 0 to indicate the connection
    has been closed. Use stream_receive to receive any remaining bytes if required. If an
    error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    buffer         = a pointer to an array of t_utf16_char's in which the received characters
                     will be stored. This buffer should contain at least 2 elements to
                     ensure that a full UTF-16 character is received (UTF-16 characters may
                     be up to 2 code units or words in length).
    max_code_units = the size of the buffer in code units (words)

    Return value:

    The number of code units written to the caller's buffer. If fewer bytes than the size
    of a character are available and the connection has been closed gracefully then 0 is
    returned. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_receive_utf16_chars(t_stream stream, t_utf16_char * buffer, t_int max_code_units);

/*
    Name:   stream_receive_utf16_char_array

    Description:

    This function receives an array of UTF-8 characters over a client stream. It returns
    exactly the number of code units requested. Since UTF-16 characters are variable-sized 
    and may be up to two code units (words) long, this function may return an error if the
    characters received do not fit exactly within the number of code units requested, since
    it only extracts complete UTF-16 characters from the input stream. The return value is 1
    if the entire array is received. It returns a negative error code if an error occurs.
    If the peer closes the stream gracefully then zero is returned.

    If the buffer is too small for a UTF-16 character (possible if last character
    does not fit in buffer) then -QERR_BUFFER_TOO_SMALL is returned and nothing is read from
    the input stream. Hence, a subsequent call with a larger buffer can succeed in
    reading the UTF-16 character.

    If an illegal UTF-8 character is encountered then -QERR_ILLEGAL_UTF8_CHARACTER is
    returned.

    If an illegal UTF-16 character is encountered then -QERR_ILLEGAL_UTF16_CHARACTER is
    returned.

    If an illegal UTF-32 character is encountered then -QERR_ILLEGAL_UTF32_CHARACTER is
    returned.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then 
    this function will swap the order of the bytes that it receives before storing them
    in the given buffer.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the number of
    code units specified then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns the number of code units returned in the caller's buffer.
    This number may be smaller than the number requested even if all the requested bytes
    are available because this function never reads a partial character. Thus, there may
    be 1-3 bytes unused at the end of the buffer if the next character would occupy more
    than the bytes available.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of a character. Otherwise it returns the number
    of characters read before the connection closed. Once there are fewer bytes left to
    receive than the size of a character then it will return 0 to indicate the connection
    has been closed. Use stream_receive to receive any remaining bytes if required. If an
    error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    buffer         = a pointer to an array of t_utf16_char's in which the received characters
                     will be stored.
    max_code_units = the size of the buffer in code units (16-bit words)

    Return value:

    Returns 1 upon success. If fewer code units than the size of the array are available and the 
    connection has been closed gracefully then 0 is returned. If an error occurs then a 
    negative error code is returned.
*/
EXTERN t_int
stream_receive_utf16_char_array(t_stream stream, t_utf16_char * buffer, t_int max_code_units);

/*
    Name:   stream_receive_utf32_char

    Description:

    This function receives a full UTF-32 character over a client stream. Since UTF-32
    characters are fixed-sized and are always one code unit (double word) long, this
    function always returns one double word, extracting a complete UTF-32 character
    from the input stream. The return value is the number of code units (double words) 
    stored in the buffer.

    If an illegal UTF-8 character is encountered then -QERR_ILLEGAL_UTF8_CHARACTER is
    returned. If the lead byte was valid then the number of bytes indicated by the lead
    byte (1-4) are removed from the input stream. If the lead byte was not valid then
    only the invalid lead byte is removed from the input stream.

    If an illegal UTF-16 character is encountered then -QERR_ILLEGAL_UTF16_CHARACTER is
    returned. If the lead code unit was valid then the number of words indicated by the lead
    code unit (1-2) are removed from the input stream. If the lead code unit was not valid then
    only the invalid lead code unit is removed from the input stream.

    If an illegal UTF-32 character is encountered then -QERR_ILLEGAL_UTF32_CHARACTER is
    returned. The invalid character is removed from the input stream.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then 
    this function will swap the order of the bytes that it receives before storing them
    in the given buffer. Note that stream_set_character_format changes the swap bytes setting
    of the stream if the STREAM_FORMAT_AUTO format is used and it detects a different byte
    order in the UTF byte-order marker (BOM).

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    a character then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns the number of characters received.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of a character. Otherwise it returns the number
    of characters read before the connection closed. Once there are fewer bytes left to
    receive than the size of a character then it will return 0 to indicate the connection
    has been closed. Use stream_receive to receive any remaining bytes if required. If an
    error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream      = a client stream established using stream_connect or stream_accept.
    buffer      = a pointer to a t_utf32_char variable in which the received character
                  will be stored.

    Return value:

    The number of characters received, which may be less than the requested number 
    for non-blocking streams. If fewer bytes than the size of a character are available
    and the connection has been closed gracefully then 0 is returned. If an error occurs
    then a negative error code is returned.
*/
EXTERN t_int
stream_receive_utf32_char(t_stream stream, t_utf32_char * buffer);

/*
    Name:   stream_receive_utf32_chars

    Description:

    This function receives an array of UTF-32 characters over a client stream.
    
    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of a
    UTF-32 character then it returns -QERR_WOULD_BLOCK. In this regard, it differs from
    the stream_receive_utf32_char_array function which returns -QERR_WOULD_BLOCK if the
    entire array cannot be returned without blocking.
    
    When -QERR_WOULD_BLOCK is returned, the stream_poll function may be used with the
    STREAM_POLL_RECEIVE flag to determine when data becomes available. Otherwise this
    functions returns the number of code units returned in the caller's buffer.

    If stream_listen or stream_connect was called with the non-blocking flag set to
    false (0), then this function attempts to return the number of code units requested
    unless the stream is closed by the peer before the full quantity is received.
    
    The return value is the number of code units (dwords) stored in the supplied buffer.

    If an illegal UTF-8 character is encountered then -QERR_ILLEGAL_UTF8_CHARACTER is
    returned. If the lead byte was valid then the number of bytes indicated by the lead
    byte (1-4) are removed from the input stream. If the lead byte was not valid then
    only the invalid lead byte is removed from the input stream.

    If an illegal UTF-16 character is encountered then -QERR_ILLEGAL_UTF16_CHARACTER is
    returned. If the lead code unit was valid then the number of words indicated by the lead
    code unit (1-2) are removed from the input stream. If the lead code unit was not valid then
    only the invalid lead code unit is removed from the input stream.

    If an illegal UTF-32 character is encountered then -QERR_ILLEGAL_UTF32_CHARACTER is
    returned. The invalid character is removed from the input stream.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then 
    this function will swap the order of the bytes that it receives before storing them
    in the given buffer.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of a character. Otherwise it returns the number
    of code units read before the connection closed. Once there are fewer bytes left to
    receive than the size of a character then it will return 0 to indicate the connection
    has been closed. Use stream_receive to receive any remaining bytes if required. If an
    error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    buffer         = a pointer to an array of t_utf32_char's in which the received characters
                     will be stored.
    max_code_units = the size of the buffer in code units (dwords)

    Return value:

    The number of code units written to the caller's buffer. If fewer bytes than the size
    of a character are available and the connection has been closed gracefully then 0 is
    returned. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_receive_utf32_chars(t_stream stream, t_utf32_char * buffer, t_int max_code_units);

/*
    Name:   stream_receive_utf32_char_array

    Description:

    This function receives an array of UTF-32 characters over a client stream. It returns
    exactly the number of code units requested. The return value is 1 if the entire array 
    is received. It returns a negative error code if an error occurs. If the peer closes 
    the stream gracefully then zero is returned.

    If an illegal UTF-8 character is encountered then -QERR_ILLEGAL_UTF8_CHARACTER is
    returned.

    If an illegal UTF-16 character is encountered then -QERR_ILLEGAL_UTF16_CHARACTER is
    returned.

    If an illegal UTF-32 character is encountered then -QERR_ILLEGAL_UTF32_CHARACTER is
    returned.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then 
    this function will swap the order of the bytes that it receives before storing them
    in the given buffer.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the number of
    code units specified then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns the number of code units returned in the caller's buffer.
    This number may be smaller than the number requested even if all the requested bytes
    are available because this function never reads a partial character. Thus, there may
    be 1-3 bytes unused at the end of the buffer if the next character would occupy more
    than the bytes available.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of a character. Otherwise it returns the number
    of characters read before the connection closed. Once there are fewer bytes left to
    receive than the size of a character then it will return 0 to indicate the connection
    has been closed. Use stream_receive to receive any remaining bytes if required. If an
    error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    buffer         = a pointer to an array of t_utf32_char's in which the received characters
                     will be stored.
    max_code_units = the size of the buffer in code units (32-bit words)

    Return value:

    Returns 1 upon success. If fewer code units than the size of the array are available and the 
    connection has been closed gracefully then 0 is returned. If an error occurs then a 
    negative error code is returned.
*/
EXTERN t_int
stream_receive_utf32_char_array(t_stream stream, t_utf32_char * buffer, t_int max_code_units);

/*
    Name:   stream_receive_short

    Description:

    This function receives a 16-bit integer over a client stream. If the stream has
    been configured to swap bytes using stream_set_swap_bytes then this function will
    swap the order of the bytes that it receives before storing them in the given
    buffer.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    a short integer then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns the number of short integers received.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of a short integer. Otherwise it returns the number
    of short integers read before the connection closed. Once there are fewer bytes left to
    receive than the size of a short integer then it will return 0 to indicate the connection
    has been closed. Use stream_receive to receive any remaining bytes if required. If an
    error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream      = a client stream established using stream_connect or stream_accept.
    value       = a pointer to a t_short variable in which the received data will be stored.

    Return value:

    The number of short integers received, which may be less than the requested number 
    for non-blocking streams. If fewer bytes than the size of a short integer are available
    and the connection has been closed gracefully then 0 is returned. If an error occurs
    then a negative error code is returned.
*/
EXTERN t_int
stream_receive_short(t_stream stream, t_short * value);

/*
    Name:   stream_receive_shorts

    Description:

    This function receives an array of 16-bit integers over a client stream. If 
    the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each short integer that it
    receives before storing them in the given buffer.

    Unlike the stream_receive_short_array function, this function does not require that the
    stream receive buffer be at least num_elements 16-bit integers in length. Hence, it
    allows smaller stream buffers to be used.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    a short integer then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns the number of short integers received.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of a short integer. Otherwise it returns the number
    of short integers read before the connection closed. Once there are fewer bytes left to
    receive than the size of a short integer then it will return 0 to indicate the connection
    has been closed. Use stream_receive to receive any remaining bytes if required. If an
    error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = a pointer to an array of t_short variables in which the received 
                   data will be stored. It must be at least num_elements in length.
    num_elements = the number of elements to receive

    Return value:

    The number of short integers received, which may be less than the requested number 
    for non-blocking streams. If fewer bytes than size of a short integer are available
    and the connection has been closed gracefully then 0 is returned. If an error occurs
    then a negative error code is returned.
*/
EXTERN t_int
stream_receive_shorts(t_stream stream, t_short * elements, t_uint num_elements);

/*
    Name:   stream_receive_short_array

    Description:

    This function receives an array of 16-bit integers over a client stream. It differs
    from the stream_receive_shorts function in that it treats the entire array as an
    atomic unit. It either receives all of the array or none of it. If the stream
    has been configured to swap bytes using stream_set_swap_bytes then this function
    will swap the order of the bytes within each short integer that it receives before
    storing them in the given buffer.

    Unlike the stream_receive_shorts function, the size of the stream receive buffer 
    must be at least as large as the number of 16-bit integers being received.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    the entire array then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of the entire array. Otherwise it returns 1. Once
    there are fewer bytes left to receive than the size of the entire array then it will
    return 0 to indicate the connection has been closed. Use stream_receive to receive any
    remaining bytes if required. If an error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = a pointer to a t_short array in which the received data will be stored.
    num_elements = the number of elements to receive, and available in the elements array.

    Return value:

    Returns 1 on success. If not enough data is available and the connection has been
    closed gracefully then 0 is returned. If an error occurs then a negative error code
    is returned.
*/
EXTERN t_int
stream_receive_short_array(t_stream stream, t_short * elements, t_uint num_elements);

/*
    Name:   stream_receive_int

    Description:

    This function receives a 32-bit integer over a client stream. If the stream has
    been configured to swap bytes using stream_set_swap_bytes then this function will
    swap the order of the bytes that it receives before storing them in the given
    buffer.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    an integer then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns the number of integers received.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of an integer. Otherwise it returns the number
    of integers read before the connection closed. Once there are fewer bytes left to
    receive than the size of an integer then it will return 0 to indicate the connection
    has been closed. Use stream_receive to receive any remaining bytes if required. If an
    error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream      = a client stream established using stream_connect or stream_accept.
    value       = a pointer to a t_int variable in which the received data will be stored.

    Return value:

    The number of integers received, which may be less than the requested number 
    for non-blocking streams. If fewer bytes than the size of an integer are available
    and the connection has been closed gracefully then 0 is returned. If an error occurs
    then a negative error code is returned.
*/
EXTERN t_int
stream_receive_int(t_stream stream, t_int * value);

/*
    Name:   stream_receive_ints

    Description:

    This function receives an array of 32-bit integers over a client stream. If 
    the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each integer that it
    receives before storing them in the given buffer.

    Unlike the stream_receive_int_array function, this function does not require that the
    stream receive buffer be at least num_elements 32-bit integers in length. Hence, it
    allows smaller stream buffers to be used.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    an integer then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns the number of integers received.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of an integer. Otherwise it returns the number
    of integers read before the connection closed. Once there are fewer bytes left to
    receive than the size of an integer then it will return 0 to indicate the connection
    has been closed. Use stream_receive to receive any remaining bytes if required. If an
    error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = a pointer to an array of t_int variables in which the received 
                   data will be stored. It must be at least num_elements in length.
    num_elements = the number of elements to receive

    Return value:

    The number of integers received, which may be less than the requested number 
    for non-blocking streams. If fewer bytes than the size of an integer are available
    and the connection has been closed gracefully then 0 is returned. If an error occurs
    then a negative error code is returned.
*/
EXTERN t_int
stream_receive_ints(t_stream stream, t_int * elements, t_uint num_elements);

/*
    Name:   stream_receive_int_array

    Description:

    This function receives an array of 32-bit integers over a client stream. It differs
    from the stream_receive_ints function in that it treats the entire array as an
    atomic unit. It either receives all of the array or none of it. If the stream
    has been configured to swap bytes using stream_set_swap_bytes then this function
    will swap the order of the bytes within each int integer that it receives before
    storing them in the given buffer.

    Unlike the stream_receive_ints function, the size of the stream receive buffer 
    must be at least as large as the number of 32-bit integers being received.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    the entire array then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of the entire array. Otherwise it returns 1. Once
    there are fewer bytes left to receive than the size of the entire array then it will
    return 0 to indicate the connection has been closed. Use stream_receive to receive any
    remaining bytes if required. If an error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = a pointer to a t_int array in which the received data will be stored.
    num_elements = the number of elements to receive, and available in the elements array.

    Return value:

    Returns 1 on success. If not enough data is available and the connection has been
    closed gracefully then 0 is returned. If an error occurs then a negative error code
    is returned.
*/
EXTERN t_int
stream_receive_int_array(t_stream stream, t_int * elements, t_uint num_elements);

/*
    Name:   stream_receive_long

    Description:

    This function receives a 64-bit integer over a client stream. If the stream has
    been configured to swap bytes using stream_set_swap_bytes then this function will
    swap the order of the bytes that it receives before storing them in the given
    buffer.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    a long integer then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns the number of long integers received.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of a long integer. Otherwise it returns the number
    of long integers read before the connection closed. Once there are fewer bytes left to
    receive than the size of a long integer then it will return 0 to indicate the connection
    has been closed. Use stream_receive to receive any remaining bytes if required. If an
    error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream      = a client stream established using stream_connect or stream_accept.
    value       = a pointer to a t_long variable in which the received data will be stored.

    Return value:

    The number of long integers received, which may be less than the requested number 
    for non-blocking streams. If fewer bytes than the size of a long integer are available
    and the connection has been closed gracefully then 0 is returned. If an error occurs
    then a negative error code is returned.
*/
EXTERN t_int
stream_receive_long(t_stream stream, t_long * value);

/*
    Name:   stream_receive_longs

    Description:

    This function receives an array of 64-bit integers over a client stream. If 
    the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each long integer that it
    receives before storing them in the given buffer.

    Unlike the stream_receive_long_array function, this function does not require that the
    stream receive buffer be at least num_elements 64-bit integers in length. Hence, it
    allows smaller stream buffers to be used.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    a long integer then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns the number of long integers received.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of a long integer. Otherwise it returns the number
    of long integers read before the connection closed. Once there are fewer bytes left to
    receive than the size of a long integer then it will return 0 to indicate the connection
    has been closed. Use stream_receive to receive any remaining bytes if required. If an
    error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = a pointer to an array of t_long variables in which the received 
                   data will be stored. It must be at least num_elements in length.
    num_elements = the number of elements to receive

    Return value:

    The number of long integers received, which may be less than the requested number 
    for non-blocking streams. If fewer bytes than size of a long integer are available
    and the connection has been closed gracefully then 0 is returned. If an error occurs
    then a negative error code is returned.
*/
EXTERN t_int
stream_receive_longs(t_stream stream, t_long * elements, t_uint num_elements);

/*
    Name:   stream_receive_long_array

    Description:

    This function receives an array of 64-bit integers over a client stream. It differs
    from the stream_receive_longs function in that it treats the entire array as an
    atomic unit. It either receives all of the array or none of it. If the stream
    has been configured to swap bytes using stream_set_swap_bytes then this function
    will swap the order of the bytes within each long integer that it receives before
    storing them in the given buffer.

    Unlike the stream_receive_longs function, the size of the stream receive buffer 
    must be at least as large as the number of 64-bit integers being received.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    the entire array then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of the entire array. Otherwise it returns 1. Once
    there are fewer bytes left to receive than the size of the entire array then it will
    return 0 to indicate the connection has been closed. Use stream_receive to receive any
    remaining bytes if required. If an error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = a pointer to a t_long array in which the received data will be stored.
    num_elements = the number of elements to receive, and available in the elements array.

    Return value:

    Returns 1 on success. If not enough data is available and the connection has been
    closed gracefully then 0 is returned. If an error occurs then a negative error code
    is returned.
*/
EXTERN t_int
stream_receive_long_array(t_stream stream, t_long * elements, t_uint num_elements);

/*
    Name:   stream_receive_single

    Description:

    This function receives a 32-bit, single-precision, floating-point number over a client stream.
    If the stream has been configured to swap bytes using stream_set_swap_bytes then this function
    will swap the order of the bytes that it receives before storing them in the given
    buffer.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of a
    single-precision floating-point number then it returns -QERR_WOULD_BLOCK. In this case,
    the stream_poll function may be used with the STREAM_POLL_RECEIVE flag to determine when
    data becomes available. Otherwise it returns the number of single-precision floating-point
    numbers received.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of a single-precision floating-point number. Otherwise
    it returns the number of single-precision floating-point numbers read before the connection
    closed. Once there are fewer bytes left to receive than the size of a single-precision
    floating-point number then it will return 0 to indicate the connection has been closed.
    Use stream_receive to receive any remaining bytes if required. If an error occurs, then
    it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream      = a client stream established using stream_connect or stream_accept.
    value       = a pointer to a t_single variable in which the received data will be stored.

    Return value:

    The number of single-precision floating-point numbers received, which may be less than the
    requested number for non-blocking streams. If fewer bytes than the size of a single-precision
    floating-point number are available and the connection has been closed gracefully then 0 is
    returned. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_receive_single(t_stream stream, t_single * value);

/*
    Name:   stream_receive_singles

    Description:

    This function receives an array of 32-bit, single-precision, floating-point numbers over a
    client stream. If the stream has been configured to swap bytes using stream_set_swap_bytes
    then this function will swap the order of the bytes within each single-precision
    floating-point number that it receives before storing them in the given buffer.

    Unlike the stream_receive_single_array function, this function does not require that the
    stream receive buffer be at least num_elements 32-bit floats in length. Hence, it
    allows smaller stream buffers to be used.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    a single-precision floating-point number then it returns -QERR_WOULD_BLOCK. In this case,
    the stream_poll function may be used with the STREAM_POLL_RECEIVE flag to determine when
    data becomes available. Otherwise it returns the number of single-precision floating-point
    numbers received.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of a single-precision floating-point number. Otherwise
    it returns the number of single-precision floating-point numbers read before the connection
    closed. Once there are fewer bytes left to receive than the size of a single-precision
    floating-point number then it will return 0 to indicate the connection has been closed.
    Use stream_receive to receive any remaining bytes if required. If an error occurs, then
    it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = a pointer to an array of t_single variables in which the received 
                   data will be stored. It must be at least num_elements in length.
    num_elements = the number of elements to receive

    Return value:

    The number of single-precision floating-point numbers received, which may be less than
    the requested number for non-blocking streams. If fewer bytes than size of a single-precision
    floating-point number are available and the connection has been closed gracefully then 0
    is returned. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_receive_singles(t_stream stream, t_single * elements, t_uint num_elements);

/*
    Name:   stream_receive_single_array

    Description:

    This function receives an array of 32-bit, single-precision, floating-point numbers
    over a client stream. It differs from the stream_receive_singles function in that
    it treats the entire array as an atomic unit. It either receives all of the array
    or none of it. If the stream has been configured to swap bytes using stream_set_swap_bytes
    then this function will swap the order of the bytes within each single-precision,
    floating-point number that it receives before storing them in the given buffer.

    Unlike the stream_receive_singles function, the size of the stream receive buffer 
    must be at least as large as the number of 32-bit floats being received.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    the entire array then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of the entire array. Otherwise it returns 1. Once
    there are fewer bytes left to receive than the size of the entire array then it will
    return 0 to indicate the connection has been closed. Use stream_receive to receive any
    remaining bytes if required. If an error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = a pointer to a t_single array in which the received data will be stored.
    num_elements = the number of elements to receive, and available in the elements array.

    Return value:

    Returns 1 on success. If not enough data is available and the connection has been
    closed gracefully then 0 is returned. If an error occurs then a negative error code
    is returned.
*/
EXTERN t_int
stream_receive_single_array(t_stream stream, t_single * elements, t_uint num_elements);

/*
    Name:   stream_receive_double

    Description:

    This function receives a 64-bit, double-precision, floating-point number over a client stream.
    If the stream has been configured to swap bytes using stream_set_swap_bytes then this function
    will swap the order of the bytes that it receives before storing them in the given
    buffer.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of a
    double-precision floating-point number then it returns -QERR_WOULD_BLOCK. In this case,
    the stream_poll function may be used with the STREAM_POLL_RECEIVE flag to determine when
    data becomes available. Otherwise it returns the number of double-precision floating-point
    numbers received.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of a double-precision floating-point number. Otherwise
    it returns the number of double-precision floating-point numbers read before the connection
    closed. Once there are fewer bytes left to receive than the size of a double-precision
    floating-point number then it will return 0 to indicate the connection has been closed.
    Use stream_receive to receive any remaining bytes if required. If an error occurs, then
    it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream      = a client stream established using stream_connect or stream_accept.
    value       = a pointer to a t_double variable in which the received data will be stored.

    Return value:

    The number of double-precision floating-point numbers received, which may be less than the
    requested number for non-blocking streams. If fewer bytes than the size of a double-precision
    floating-point number are available and the connection has been closed gracefully then 0 is
    returned. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_receive_double(t_stream stream, t_double * value);

/*
    Name:   stream_receive_doubles

    Description:

    This function receives an array of 64-bit, double-position, floating-point numbers over a
    client stream. If the stream has been configured to swap bytes using stream_set_swap_bytes
    then this function will swap the order of the bytes within each double-precision
    floating-point number that it receives before storing them in the given buffer.

    Unlike the stream_receive_double_array function, this function does not require that the
    stream receive buffer be at least num_elements 64-bit floats in length. Hence, it
    allows smaller stream buffers to be used.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    a double-precision floating-point number then it returns -QERR_WOULD_BLOCK. In this case,
    the stream_poll function may be used with the STREAM_POLL_RECEIVE flag to determine when
    data becomes available. Otherwise it returns the number of double-precision floating-point
    numbers received.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of a double-precision floating-point number. Otherwise
    it returns the number of double-precision floating-point numbers read before the connection
    closed. Once there are fewer bytes left to receive than the size of a double-precision
    floating-point number then it will return 0 to indicate the connection has been closed.
    Use stream_receive to receive any remaining bytes if required. If an error occurs, then
    it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = a pointer to an array of t_double variables in which the received 
                   data will be stored. It must be at least num_elements in length.
    num_elements = the number of elements to receive

    Return value:

    The number of double-precision floating-point numbers received, which may be less than
    the requested number for non-blocking streams. If fewer bytes than size of a double-precision
    floating-point number are available and the connection has been closed gracefully then 0
    is returned. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_receive_doubles(t_stream stream, t_double * elements, t_uint num_elements);

/*
    Name:   stream_receive_double_array

    Description:

    This function receives an array of 64-bit, double-position, floating-point numbers
    over a client stream. It differs from the stream_receive_doubles function in that
    it treats the entire array as an atomic unit. It either receives all of the array
    or none of it. If the stream has been configured to swap bytes using stream_set_swap_bytes
    then this function will swap the order of the bytes within each double-position,
    floating-point number that it receives before storing them in the given buffer.

    Unlike the stream_receive_doubles function, the size of the stream receive buffer 
    must be at least as large as the number of 64-bit floats being received.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    the entire array then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of the entire array. Otherwise it returns 1. Once
    there are fewer bytes left to receive than the size of the entire array then it will
    return 0 to indicate the connection has been closed. Use stream_receive to receive any
    remaining bytes if required. If an error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = a pointer to a t_double array in which the received data will be stored.
    num_elements = the number of elements to receive, and available in the elements array.

    Return value:

    Returns 1 on success. If not enough data is available and the connection has been
    closed gracefully then 0 is returned. If an error occurs then a negative error code
    is returned.
*/
EXTERN t_int
stream_receive_double_array(t_stream stream, t_double * elements, t_uint num_elements);

/*
    Name:   stream_receive_unit

    Description:

    This function receives a "unit" over a client stream, where the size of a "unit" is
    determined by the unit_size parameter. If the stream has been configured to swap bytes
    using stream_set_swap_bytes then this function will swap the order of the bytes that
    it receives before storing them in the given buffer.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of a
    unit then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll function may be
    used with the STREAM_POLL_RECEIVE flag to determine when data becomes available.
    Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of a unit. Otherwise it returns 1. Once there are 
    fewer bytes left to receive than the size of a unit then it will return 0 to indicate 
    the connection has been closed. Use stream_receive to receive any remaining bytes if 
    required. If an error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream      = a client stream established using stream_connect or stream_accept.
    unit        = a pointer to a variable in which the received data will be stored. It
                  must be unit_size bytes in size.
    unit_size   = the size of a unit

    Return value:

    The number of units received, which may be less than the requested number for non-blocking
    streams. If fewer bytes than the size of a unit are available and the connection has been
    closed gracefully then 0 is returned. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_receive_unit(t_stream stream, void * unit, t_int unit_size);

/*
    Name:   stream_receive_units

    Description:

    This function receives an array of "units" over a client stream, where the size of a "unit"
    is determined by the unit_size parameter. If the stream has been configured to swap bytes
    using stream_set_swap_bytes then this function will swap the order of the bytes within each
    unit that it receives before storing them in the given buffer.

    Unlike the stream_receive_unit_array function, this function does not require that the
    stream receive buffer be at least num_units * unit_size bytes in length. Hence, it
    allows smaller stream buffers to be used.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    a unit then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll function may be
    used with the STREAM_POLL_RECEIVE flag to determine when data becomes available. Otherwise
    it returns the number of units received.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of a unit. Otherwise it returns the number of units
    read before the connection closed. Once there are fewer bytes left to receive than the
    size of a unit then it will return 0 to indicate the connection has been closed.
    Use stream_receive to receive any remaining bytes if required. If an error occurs, then
    it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    units        = a pointer to an array of units in which the received data will be stored. 
                   It must be at least num_units in length.
    unit_size    = the size of a unit in bytes
    num_units    = the number of units to receive

    Return value:

    The number of units received, which may be less than the requested number for non-blocking
    streams. If fewer bytes than size of a unit are available and the connection has been
    closed gracefully then 0 is returned. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_receive_units(t_stream stream, void * units, t_int unit_size, t_int num_units);

/*
    Name:   stream_receive_unit_array

    Description:

    This function receives an array of "units" over a client stream, where the size of a "unit"
    is determined by the unit_size parameter. It differs from the stream_receive_units function
    in that it treats the entire array as an atomic entity. It either receives all of the array
    or none of it. If the stream has been configured to swap bytes using stream_set_swap_bytes
    then this function will swap the order of the bytes within each unit that it receives
    before storing them in the given buffer.

    Unlike the stream_receive_units function, the size of the stream receive buffer 
    must be at least num_units * unit_size bytes in length.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is read.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    the entire array then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to receive than the size of the entire array. Otherwise it returns 1. Once
    there are fewer bytes left to receive than the size of the entire array then it will
    return 0 to indicate the connection has been closed. Use stream_receive to receive any
    remaining bytes if required. If an error occurs, then it returns a negative error code.

    This function does not support two threads receiving data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    units        = a pointer to an array of units in which the received data will be stored. 
                   It must be at least num_units in length.
    unit_size    = the size of a unit in bytes
    num_units    = the number of units to receive

    Return value:

    Returns 1 on success. If not enough data is available and the connection has been
    closed gracefully then 0 is returned. If an error occurs then a negative error code
    is returned.
*/
EXTERN t_int
stream_receive_unit_array(t_stream stream, void * units, t_int unit_size, t_int num_units);

/*
    Name:   stream_send

    Description:

    This function writes data to the stream buffer. It attempts to store buffer_size bytes
    in the stream buffer. If there is enough room available in the stream buffer then
    it stores the data in the buffer and returns immediately. The data is not written to
    the actual communication channel until the stream is flushed using stream_flush or
    there is no more room available in the stream buffer. If an error occurs, then it returns
    a negative error code. If the connection is closed it is considered an error condition.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and the total number of bytes sent is returned. Some of the data may remain
    in the stream buffer and not be sent until the next time stream_flush is called or
    there is no more room available in the stream buffer. If an error occurs then the
    error code is returned and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns the number of bytes sent successfully,
    which will be between 1 and the buffer_size (unless buffer_size is zero). If no bytes
    could be sent without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs
    then the error code is returned and the stream should be closed.

    This function does not support two threads calling stream_send or stream_flush at the same
    time. However, stream_send or stream_flush may be called by another thread at the same time
    as stream_receive.

    The semantics of this function are comparable to the BSD send() socket function.

    Parameters:

    stream      = a client stream established using stream_connect or stream_accept.
    buffer      = a buffer of at least buffer_size bytes containing the data to be sent.
    buffer_size = the number of bytes to send from the buffer.

    Return value:

    The number of bytes sent, which may be less than buffer_size bytes for non-blocking
    streams. If an error occurs then a negative error code is returned. A value of zero 
    is only returned if the buffer_size is zero.
*/
EXTERN t_int
stream_send(t_stream stream, const void * buffer, t_int buffer_size);

/*
    Name:   stream_send_byte

    Description:

    This function writes a single byte to the stream buffer. It is has the
    same semantics as the stream_send function, except that it only sends 
    a single byte.

    Parameters:

    stream = a client stream established using stream_connect or stream_accept.
    value  = the value to be sent.

    Return value:

    The number of bytes sent, which will always be 1. If an error occurs then a
    negative error code is returned.
*/
EXTERN t_int
stream_send_byte(t_stream stream, t_byte value);

/*
    Name:   stream_send_bytes

    Description:

    This function writes an array of bytes to the stream buffer. It is has the
    same semantics as the stream_send function.

    Unlike the stream_send_byte_array function, this function does not require that the
    stream send buffer be at least num_elements bytes in length. Hence, it
    allows smaller stream buffers to be used.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = the array to be sent.
    num_elements = the number of elements in the array to send

    Return value:

    The number of bytes sent, which may be less than the request number of bytes
    for non-blocking streams. If an error occurs then a negative error code is
    returned.
*/
EXTERN t_int
stream_send_bytes(t_stream stream, const t_byte * elements, t_uint num_elements);

/*
    Name:   stream_send_byte_array

    Description:

    This function writes an array of bytes to the stream buffer. It attempts
    to store the bytes in the stream buffer. It differs from the stream_send_bytes
    function in that it treats the entire array as an atomic unit. It either writes
    all of the array or none of it. If there is enough room available in the stream
    buffer then it stores the data in the buffer and returns immediately. The data
    is not written to the actual communication channel until the stream is flushed
    using stream_flush or there is no more room available in the stream buffer. If
    an error occurs, then it returns a negative error code. If the connection is
    closed it is considered an error condition.

    Unlike the stream_send_bytes function, the size of the stream send buffer 
    must be at least as large as the number of bytes being sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and 1 is returned. Some of the data may remain in the stream buffer and not be
    sent until the next time stream_flush is called or there is no more room available in
    the stream buffer. If an error occurs then the error code is returned and the stream
    should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 if the array is sent successfully.
    If the array could not be sent without blocking, then -QERR_WOULD_BLOCK is returned.
    If an error occurs then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = the array to be sent.
    num_elements = the number of elements in the array to send

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_byte_array(t_stream stream, const t_byte * elements, t_uint num_elements);

/*
    Name:   stream_send_utf8_char

    Description:

    This function writes a UTF-8 character to the stream buffer. The character is converted
    according to the stream's character format prior to writing the character to the stream
    buffer. It attempts to store the converted character in the stream buffer. If there is 
    enough room available in the stream buffer then it stores the data in the buffer and
    returns immediately. The data is not written to the actual communication channel until
    the stream is flushed using stream_flush or there is no more room available in the stream 
    buffer. If an error occurs, then it returns a negative error code. If the connection is closed
    it is considered an error condition.

    Note that this function only writes complete characters to the stream. It will not write
    some but not all of the code units comprising a multi-unit character.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the converted character if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and the total number of code units read from the caller's buffer is returned.
    Some of the data may remain in the stream buffer and not be sent until the next time 
    stream_flush is called or there is no more room available in the stream buffer. If an 
    error occurs then the error code is returned and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns the number of code units read from the
    caller's buffer and sent successfully after conversion. If the character could not be
    sent without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs then the
    error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    buffer         = a pointer to an array of t_utf8_char's containing the character
                     to send. A UTF-8 character may take up to 4 code units or bytes.
    max_code_units = the size of the buffer in code units (bytes)

    Return value:

    The number of code units read from the caller's buffer and sent successfully 
    after conversion. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_utf8_char(t_stream stream, const t_utf8_char * buffer, t_int max_code_units);

/*
    Name:   stream_send_utf8_chars

    Description:

    This function writes UTF-8 characters to the stream buffer. The characters are converted
    according to the stream's character format prior to writing the character to the stream
    buffer. It attempts to store the converted characters in the stream buffer. If there is 
    enough room available in the stream buffer then it stores the data in the buffer and
    returns immediately. The data is not written to the actual communication channel until
    the stream is flushed using stream_flush or there is no more room available in the stream 
    buffer. If an error occurs, then it returns a negative error code. If the connection is closed
    it is considered an error condition.

    Unlike the stream_send_utf8_char_array function, this function does not require that the
    stream send buffer be at least num_elements code units in length. Hence, it
    allows smaller stream buffers to be used.

    Note that this function only writes complete characters to the stream. It will not write
    some but not all of the code units comprising a multi-unit character.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the converted character if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and the total number of code units read from the caller's buffer is returned.
    Some of the data may remain in the stream buffer and not be sent until the next time 
    stream_flush is called or there is no more room available in the stream buffer. If an 
    error occurs then the error code is returned and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns the number of code units written to the
    caller's buffer and sent successfully after conversion. If the character could not be
    sent without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs then the
    error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    buffer         = a pointer to an array of t_utf8_char's containing the characters
                     to send. A UTF-8 character may take up to 4 code units or bytes.
    code_units     = the size of the buffer in code units (bytes)

    Return value:

    The number of code units written to the caller's buffer and sent successfully 
    after conversion. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_utf8_chars(t_stream stream, const t_utf8_char * buffer, t_int code_units);

/*
    Name:   stream_send_utf8_char_array

    Description:

    This function writes an array of UTF-8 characters to the stream buffer. The characters 
    are converted according to the stream's character format prior to writing the characters
    to the stream buffer. It attempts to store the converted characters in the stream buffer. 
    It differs from the stream_send_utf8_chars function in that it treats the entire array 
    as an atomic unit. It either writes all of the array or none of it. If there is enough 
    room available in the stream buffer then it stores the data in the buffer and returns 
    immediately. The data is not written to the actual communication channel until the stream
    is flushed using stream_flush or there is no more room available in the stream buffer. 
    If an error occurs, then it returns a negative error code. If the connection is closed 
    it is considered an error condition.

    Unlike the stream_send_utf8_chars function, the size of the stream send buffer 
    must be at least as large as the number of code units being sent.

    Note that this function only writes complete characters to the stream. It will not write
    some but not all of the code units comprising a multi-unit character.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the converted character if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and 1 is returned. Some of the data may remain in the stream buffer and not be 
    sent until the next time stream_flush is called or there is no more room available in 
    the stream buffer. If an error occurs then the error code is returned and the stream 
    should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 if the array is sent successfully. If
    the array could not be sent without blocking, then -QERR_WOULD_BLOCK is returned. If
    an error occurs then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    buffer         = a pointer to an array of t_utf8_char's containing the characters
                     to send. A UTF-8 character may take up to 4 code units or bytes.
    code_units     = the size of the buffer in code units (bytes)

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_utf8_char_array(t_stream stream, const t_utf8_char * buffer, t_int code_units);

/*
    Name:   stream_send_utf16_char

    Description:

    This function writes a UTF-16 character to the stream buffer. The character is converted
    according to the stream's character format prior to writing the character to the stream
    buffer. It attempts to store the converted character in the stream buffer. If there is 
    enough room available in the stream buffer then it stores the data in the buffer and
    returns immediately. The data is not written to the actual communication channel until
    the stream is flushed using stream_flush or there is no more room available in the stream 
    buffer. If an error occurs, then it returns a negative error code. If the connection is closed
    it is considered an error condition.

    Note that this function only writes complete characters to the stream. It will not write
    some but not all of the code units comprising a multi-unit character.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the converted character if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and the total number of code units read from the caller's buffer is returned.
    Some of the data may remain in the stream buffer and not be sent until the next time 
    stream_flush is called or there is no more room available in the stream buffer. If an 
    error occurs then the error code is returned and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns the number of code units read from the
    caller's buffer and sent successfully after conversion. If the character could not be
    sent without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs then the
    error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    buffer         = a pointer to an array of t_utf16_char's containing the character
                     to send. A UTF-16 character may take up to 2 code units or words.
    max_code_units = the size of the buffer in code units (words)

    Return value:

    The number of code units read from the caller's buffer and sent successfully 
    after conversion. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_utf16_char(t_stream stream, const t_utf16_char * buffer, t_int max_code_units);

/*
    Name:   stream_send_utf16_chars

    Description:

    This function writes UTF-16 characters to the stream buffer. The characters are converted
    according to the stream's character format prior to writing the character to the stream
    buffer. It attempts to store the converted characters in the stream buffer. If there is 
    enough room available in the stream buffer then it stores the data in the buffer and
    returns immediately. The data is not written to the actual communication channel until
    the stream is flushed using stream_flush or there is no more room available in the stream 
    buffer. If an error occurs, then it returns a negative error code. If the connection is closed
    it is considered an error condition.

    Unlike the stream_send_utf16_char_array function, this function does not require that the
    stream send buffer be at least num_elements code units in length. Hence, it
    allows smaller stream buffers to be used.

    Note that this function only writes complete characters to the stream. It will not write
    some but not all of the code units comprising a multi-unit character.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the converted character if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and the total number of code units read from the caller's buffer is returned.
    Some of the data may remain in the stream buffer and not be sent until the next time 
    stream_flush is called or there is no more room available in the stream buffer. If an 
    error occurs then the error code is returned and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns the number of code units read from the
    caller's buffer and sent successfully after conversion. If the character could not be
    sent without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs then the
    error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    buffer         = a pointer to an array of t_utf16_char's containing the characters
                     to send. A UTF-16 character may take up to 2 code units or words.
    code_units     = the size of the buffer in code units (words)

    Return value:

    The number of code units read from the caller's buffer and sent successfully 
    after conversion. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_utf16_chars(t_stream stream, const t_utf16_char * buffer, t_int code_units);

/*
    Name:   stream_send_utf16_char_array

    Description:

    This function writes an array of UTF-16 characters to the stream buffer. The characters 
    are converted according to the stream's character format prior to writing the characters
    to the stream buffer. It attempts to store the converted characters in the stream buffer. 
    It differs from the stream_send_utf16_chars function in that it treats the entire array 
    as an atomic unit. It either writes all of the array or none of it. If there is enough 
    room available in the stream buffer then it stores the data in the buffer and returns 
    immediately. The data is not written to the actual communication channel until the stream
    is flushed using stream_flush or there is no more room available in the stream buffer. 
    If an error occurs, then it returns a negative error code. If the connection is closed 
    it is considered an error condition.

    Unlike the stream_send_utf16_chars function, the size of the stream send buffer 
    must be at least as large as the number of code units being sent.

    Note that this function only writes complete characters to the stream. It will not write
    some but not all of the code units comprising a multi-unit character.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the converted character if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and 1 is returned. Some of the data may remain in the stream buffer and not be 
    sent until the next time stream_flush is called or there is no more room available in 
    the stream buffer. If an error occurs then the error code is returned and the stream 
    should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 if the array is sent successfully. If
    the array could not be sent without blocking, then -QERR_WOULD_BLOCK is returned. If
    an error occurs then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    buffer         = a pointer to an array of t_utf16_char's containing the characters
                     to send. A UTF-16 character may take up to 2 code units or words.
    code_units     = the size of the buffer in code units (words)

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_utf16_char_array(t_stream stream, const t_utf16_char * buffer, t_int code_units);

/*
    Name:   stream_send_utf32_char

    Description:

    This function writes a UTF-32 character to the stream buffer. The character is converted
    according to the stream's character format prior to writing the character to the stream
    buffer. It attempts to store the converted character in the stream buffer. If there is 
    enough room available in the stream buffer then it stores the data in the buffer and
    returns immediately. The data is not written to the actual communication channel until
    the stream is flushed using stream_flush or there is no more room available in the stream 
    buffer. If an error occurs, then it returns a negative error code. If the connection is closed
    it is considered an error condition.

    Note that this function only writes complete characters to the stream. It will not write
    some but not all of the code units comprising a multi-unit character.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the converted character if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and the total number of code units read from the caller's buffer is returned (1).
    Some of the data may remain in the stream buffer and not be sent until the next time 
    stream_flush is called or there is no more room available in the stream buffer. If an 
    error occurs then the error code is returned and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns the number of code units read from the
    caller's buffer and sent successfully after conversion. If the character could not be
    sent without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs then the
    error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream = a client stream established using stream_connect or stream_accept.
    value  = the UTF-32 value to send.

    Return value:

    The number of code units read from the caller's buffer and sent successfully 
    after conversion (1). If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_utf32_char(t_stream stream, t_utf32_char value);

/*
    Name:   stream_send_utf32_chars

    Description:

    This function writes UTF-32 characters to the stream buffer. The characters are converted
    according to the stream's character format prior to writing the character to the stream
    buffer. It attempts to store the converted characters in the stream buffer. If there is 
    enough room available in the stream buffer then it stores the data in the buffer and
    returns immediately. The data is not written to the actual communication channel until
    the stream is flushed using stream_flush or there is no more room available in the stream 
    buffer. If an error occurs, then it returns a negative error code. If the connection is closed
    it is considered an error condition.

    Unlike the stream_send_utf32_char_array function, this function does not require that the
    stream send buffer be at least num_elements code units in length. Hence, it
    allows smaller stream buffers to be used.

    Note that this function only writes complete characters to the stream. It will not write
    some but not all of the code units comprising a multi-unit character.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the converted character if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and the total number of code units read from the caller's buffer is returned.
    Some of the data may remain in the stream buffer and not be sent until the next time 
    stream_flush is called or there is no more room available in the stream buffer. If an 
    error occurs then the error code is returned and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns the number of code units read from the
    caller's buffer and sent successfully after conversion. If the character could not be
    sent without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs then the
    error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    buffer         = a pointer to an array of t_utf32_char's containing the characters
                     to send. A UTF-32 character always takes one code unit or dword.
    code_units     = the size of the buffer in code units (dwords)

    Return value:

    The number of code units read from the caller's buffer and sent successfully 
    after conversion. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_utf32_chars(t_stream stream, const t_utf32_char * buffer, t_int code_units);

/*
    Name:   stream_send_utf32_char_array

    Description:

    This function writes an array of UTF-32 characters to the stream buffer. The characters 
    are converted according to the stream's character format prior to writing the characters
    to the stream buffer. It attempts to store the converted characters in the stream buffer. 
    It differs from the stream_send_utf32_chars function in that it treats the entire array 
    as an atomic unit. It either writes all of the array or none of it. If there is enough 
    room available in the stream buffer then it stores the data in the buffer and returns 
    immediately. The data is not written to the actual communication channel until the stream
    is flushed using stream_flush or there is no more room available in the stream buffer. 
    If an error occurs, then it returns a negative error code. If the connection is closed 
    it is considered an error condition.

    Unlike the stream_send_utf32_chars function, the size of the stream send buffer 
    must be at least as large as the number of code units being sent.

    Note that this function only writes complete characters to the stream. It will not write
    some but not all of the code units comprising a multi-unit character.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the converted character if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and 1 is returned. Some of the data may remain in the stream buffer and not be 
    sent until the next time stream_flush is called or there is no more room available in 
    the stream buffer. If an error occurs then the error code is returned and the stream 
    should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 if the array is sent successfully. If
    the array could not be sent without blocking, then -QERR_WOULD_BLOCK is returned. If
    an error occurs then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    buffer         = a pointer to an array of t_utf32_char's containing the characters
                     to send. A UTF-32 character is always one code unit or dword.
    code_units     = the size of the buffer in code units (dwords)

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_utf32_char_array(t_stream stream, const t_utf32_char * buffer, t_int code_units);

/*
    Name:   stream_send_short

    Description:

    This function writes a 16-bit short integer to the stream buffer. It attempts to
    store the short integer in the stream buffer. If there is enough room available in the
    stream buffer then it stores the data in the buffer and returns immediately. The
    data is not written to the actual communication channel until the stream is flushed
    using stream_flush or there is no more room available in the stream buffer. If an
    error occurs, then it returns a negative error code. If the connection is closed
    it is considered an error condition.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the short integer when they
    are sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and the total number of short integers sent is returned (1). Some of the data may
    remain in the stream buffer and not be sent until the next time stream_flush is called or
    there is no more room available in the stream buffer. If an error occurs then the
    error code is returned and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns the number of short integers sent successfully,
    which will be 1. If the short integer could not be sent without blocking, then
    -QERR_WOULD_BLOCK is returned. If an error occurs then the error code is returned
    and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream = a client stream established using stream_connect or stream_accept.
    value  = the value to be sent.

    Return value:

    The number of short integers sent (1). If an error occurs then a negative error
    code is returned.
*/
EXTERN t_int
stream_send_short(t_stream stream, t_short value);

/*
    Name:   stream_send_shorts

    Description:

    This function writes an array of 16-bit short integers to the stream buffer. It
    attempts to store the short integers in the stream buffer. If there is enough
    room available in the stream buffer then it stores the data in the buffer and
    returns immediately. The data is not written to the actual communication channel
    until the stream is flushed using stream_flush or there is no more room available
    in the stream buffer. If an error occurs, then it returns a negative error code.
    If the connection is closed it is considered an error condition.

    Unlike the stream_send_short_array function, this function does not require that the
    stream send buffer be at least num_elements 16-bit integers in length. Hence, it
    allows smaller stream buffers to be used.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each short integer when they
    are sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and the total number of short integers sent is returned. Some of the data may
    remain in the stream buffer and not be sent until the next time stream_flush is called or
    there is no more room available in the stream buffer. If an error occurs then the
    error code is returned and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns the number of short integers sent successfully,
    which will be between 1 and num_elements (unless num_elements is zero). If no
    short integers could be sent without blocking, then -QERR_WOULD_BLOCK is returned.
    If an error occurs then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = the array to be sent.
    num_elements = the number of elements in the array to send

    Return value:

    The number of short integers sent, which may be less than the number requested for
    non-blocking streams. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_shorts(t_stream stream, const t_short * elements, t_uint num_elements);

/*
    Name:   stream_send_short_array

    Description:

    This function writes an array of 16-bit integers to the stream buffer. It attempts
    to store the short integers in the stream buffer. It differs from the stream_send_shorts
    function in that it treats the entire array as an atomic unit. It either writes
    all of the array or none of it. If there is enough room available in the stream
    buffer then it stores the data in the buffer and returns immediately. The data
    is not written to the actual communication channel until the stream is flushed
    using stream_flush or there is no more room available in the stream buffer. If
    an error occurs, then it returns a negative error code. If the connection is
    closed it is considered an error condition.

    Unlike the stream_send_shorts function, the size of the stream send buffer 
    must be at least as large as the number of 16-bit integers being sent.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each short integer when they
    are sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and 1 is returned. Some of the data may remain in the stream buffer and not be
    sent until the next time stream_flush is called or there is no more room available in
    the stream buffer. If an error occurs then the error code is returned and the stream
    should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 if the array is sent successfully.
    If the array could not be sent without blocking, then -QERR_WOULD_BLOCK is returned.
    If an error occurs then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = the array to be sent.
    num_elements = the number of elements in the array to send

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_short_array(t_stream stream, const t_short * elements, t_uint num_elements);

/*
    Name:   stream_send_int

    Description:

    This function writes a 32-bit integer to the stream buffer. It attempts to
    store the integer in the stream buffer. If there is enough room available in the
    stream buffer then it stores the data in the buffer and returns immediately. The
    data is not written to the actual communication channel until the stream is flushed
    using stream_flush or there is no more room available in the stream buffer. If an
    error occurs, then it returns a negative error code. If the connection is closed
    it is considered an error condition.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the integer when they
    are sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and the total number of integers sent is returned (1). Some of the data may
    remain in the stream buffer and not be sent until the next time stream_flush is called or
    there is no more room available in the stream buffer. If an error occurs then the
    error code is returned and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns the number of integers sent successfully,
    which will be 1. If the integer could not be sent without blocking, then
    -QERR_WOULD_BLOCK is returned. If an error occurs then the error code is returned
    and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream = a client stream established using stream_connect or stream_accept.
    value  = the value to be sent.

    Return value:

    The number of integers sent (1). If an error occurs then a negative error
    code is returned.
*/
EXTERN t_int
stream_send_int(t_stream stream, t_int value);

/*
    Name:   stream_send_ints

    Description:

    This function writes an array of 32-bit integers to the stream buffer. It
    attempts to store the integers in the stream buffer. If there is enough
    room available in the stream buffer then it stores the data in the buffer and
    returns immediately. The data is not written to the actual communication channel
    until the stream is flushed using stream_flush or there is no more room available
    in the stream buffer. If an error occurs, then it returns a negative error code.
    If the connection is closed it is considered an error condition.

    Unlike the stream_send_int_array function, this function does not require that the
    stream send buffer be at least num_elements 32-bit integers in length. Hence, it
    allows smaller stream buffers to be used.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each integer when they
    are sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and the total number of integers sent is returned. Some of the data may
    remain in the stream buffer and not be sent until the next time stream_flush is called or
    there is no more room available in the stream buffer. If an error occurs then the
    error code is returned and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns the number of integers sent successfully,
    which will be between 1 and num_elements (unless num_elements is zero). If no
    integers could be sent without blocking, then -QERR_WOULD_BLOCK is returned.
    If an error occurs then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = the array to be sent.
    num_elements = the number of elements in the array to send

    Return value:

    The number of integers sent, which may be less than the number requested for
    non-blocking streams. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_ints(t_stream stream, const t_int * elements, t_uint num_elements);

/*
    Name:   stream_send_int_array

    Description:

    This function writes an array of 32-bit integers to the stream buffer. It attempts
    to store the integers in the stream buffer. It differs from the stream_send_ints
    function in that it treats the entire array as an atomic unit. It either writes
    all of the array or none of it. If there is enough room available in the stream
    buffer then it stores the data in the buffer and returns immediately. The data
    is not written to the actual communication channel until the stream is flushed
    using stream_flush or there is no more room available in the stream buffer. If
    an error occurs, then it returns a negative error code. If the connection is
    closed it is considered an error condition.

    Unlike the stream_send_ints function, the size of the stream send buffer 
    must be at least as large as the number of 32-bit integers being sent.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each integer when they
    are sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and 1 is returned. Some of the data may remain in the stream buffer and not be
    sent until the next time stream_flush is called or there is no more room available in
    the stream buffer. If an error occurs then the error code is returned and the stream
    should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 if the array is sent successfully.
    If the array could not be sent without blocking, then -QERR_WOULD_BLOCK is returned.
    If an error occurs then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = the array to be sent.
    num_elements = the number of elements in the array to send

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_int_array(t_stream stream, const t_int * elements, t_uint num_elements);

/*
    Name:   stream_send_long

    Description:

    This function writes a 64-bit long integer to the stream buffer. It attempts to
    store the long integer in the stream buffer. If there is enough room available in the
    stream buffer then it stores the data in the buffer and returns immediately. The
    data is not written to the actual communication channel until the stream is flushed
    using stream_flush or there is no more room available in the stream buffer. If an
    error occurs, then it returns a negative error code. If the connection is closed
    it is considered an error condition.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the long integer when they
    are sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and the total number of long integers sent is returned (1). Some of the data may
    remain in the stream buffer and not be sent until the next time stream_flush is called or
    there is no more room available in the stream buffer. If an error occurs then the
    error code is returned and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns the number of long integers sent successfully,
    which will be 1. If the long integer could not be sent without blocking, then
    -QERR_WOULD_BLOCK is returned. If an error occurs then the error code is returned
    and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream = a client stream established using stream_connect or stream_accept.
    value  = the value to be sent.

    Return value:

    The number of long integers sent (1). If an error occurs then a negative error
    code is returned.
*/
EXTERN t_int
stream_send_long(t_stream stream, t_long value);

/*
    Name:   stream_send_longs

    Description:

    This function writes an array of 64-bit long integers to the stream buffer. It
    attempts to store the long integers in the stream buffer. If there is enough
    room available in the stream buffer then it stores the data in the buffer and
    returns immediately. The data is not written to the actual communication channel
    until the stream is flushed using stream_flush or there is no more room available
    in the stream buffer. If an error occurs, then it returns a negative error code.
    If the connection is closed it is considered an error condition.

    Unlike the stream_send_long_array function, this function does not require that the
    stream send buffer be at least num_elements 64-bit integers in length. Hence, it
    allows smaller stream buffers to be used.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each long integer when they
    are sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and the total number of long integers sent is returned. Some of the data may
    remain in the stream buffer and not be sent until the next time stream_flush is called or
    there is no more room available in the stream buffer. If an error occurs then the
    error code is returned and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns the number of long integers sent successfully,
    which will be between 1 and num_elements (unless num_elements is zero). If no
    long integers could be sent without blocking, then -QERR_WOULD_BLOCK is returned.
    If an error occurs then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = the array to be sent.
    num_elements = the number of elements in the array to send

    Return value:

    The number of long integers sent, which may be less than the number requested for
    non-blocking streams. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_longs(t_stream stream, const t_long * elements, t_uint num_elements);

/*
    Name:   stream_send_long_array

    Description:

    This function writes an array of 64-bit integers to the stream buffer. It attempts
    to store the long integers in the stream buffer. It differs from the stream_send_longs
    function in that it treats the entire array as an atomic unit. It either writes
    all of the array or none of it. If there is enough room available in the stream
    buffer then it stores the data in the buffer and returns immediately. The data
    is not written to the actual communication channel until the stream is flushed
    using stream_flush or there is no more room available in the stream buffer. If
    an error occurs, then it returns a negative error code. If the connection is
    closed it is considered an error condition.

    Unlike the stream_send_longs function, the size of the stream send buffer 
    must be at least as large as the number of 64-bit integers being sent.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each long integer when they
    are sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and 1 is returned. Some of the data may remain in the stream buffer and not be
    sent until the next time stream_flush is called or there is no more room available in
    the stream buffer. If an error occurs then the error code is returned and the stream
    should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 if the array is sent successfully.
    If the array could not be sent without blocking, then -QERR_WOULD_BLOCK is returned.
    If an error occurs then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = the array to be sent.
    num_elements = the number of elements in the array to send

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_long_array(t_stream stream, const t_long * elements, t_uint num_elements);

/*
    Name:   stream_send_single

    Description:

    This function writes a 32-bit single-precision floating-point number to the stream
    buffer. It attempts to store the single-precision floating-point number in the stream
    buffer. If there is enough room available in the stream buffer then it stores the data
    in the buffer and returns immediately. The data is not written to the actual communication
    channel until the stream is flushed using stream_flush or there is no more room available
    in the stream buffer. If an error occurs, then it returns a negative error code. If the
    connection is closed it is considered an error condition.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the single-precision floating-point
    number when they are sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and the total number of single-precision floating-point numbers sent is
    returned (1). Some of the data may remain in the stream buffer and not be sent until
    the next time stream_flush is called or there is no more room available in the stream
    buffer. If an error occurs then the error code is returned and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns the number of single-precision floating-point
    numbers sent successfully, which will be 1. If the single-precision floating-point
    number could not be sent without blocking, then -QERR_WOULD_BLOCK is returned. If an error
    occurs then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream = a client stream established using stream_connect or stream_accept.
    value  = the value to be sent.

    Return value:

    The number of single-precision floating-point numbers sent (1). If an error occurs then
    a negative error code is returned.
*/
EXTERN t_int
stream_send_single(t_stream stream, t_single value);

/*
    Name:   stream_send_singles

    Description:

    This function writes an array of 32-bit single-precision floating-point numbers to
    the stream buffer. It attempts to store the single-precision floating-point numbers
    in the stream buffer. If there is enough room available in the stream buffer then
    it stores the data in the buffer and returns immediately. The data is not written
    to the actual communication channel until the stream is flushed using stream_flush
    or there is no more room available in the stream buffer. If an error occurs, then
    it returns a negative error code. If the connection is closed it is considered an
    error condition.

    Unlike the stream_send_single_array function, this function does not require that the
    stream send buffer be at least num_elements 32-bit floats in length. Hence, it
    allows smaller stream buffers to be used.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each single-precision
    floating-point number when they are sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to
    false (0), then this function may block attempting to flush the stream buffer. All
    the data will be consumed and the total number of single-precision floating-point
    numbers sent is returned. Some of the data may remain in the stream buffer and not
    be sent until the next time stream_flush is called or there is no more room available
    in the stream buffer. If an error occurs then the error code is returned and the
    stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to
    true (1), then this function does not block. It returns the number of single-precision
    floating-point numbers sent successfully, which will be between 1 and num_elements
    (unless num_elements is zero). If no single-precision floating-point numbers could
    be sent without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs
    then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = the array to be sent.
    num_elements = the number of elements in the array to send

    Return value:

    The number of single-precision floating-point numbers sent, which may be less than
    the number requested for non-blocking streams. If an error occurs then a negative
    error code is returned.
*/
EXTERN t_int
stream_send_singles(t_stream stream, const t_single * elements, t_uint num_elements);

/*
    Name:   stream_send_single_array

    Description:

    This function writes an array of 32-bit single-precision floating-point numbers to
    the stream buffer. It attempts to store the single-precision floating-point numbers
    in the stream buffer. It differs from the stream_send_singles function in that it
    treats the entire array as an atomic unit. It either writes all of the array or
    none of it. If there is enough room available in the stream buffer then it stores
    the data in the buffer and returns immediately. The data is not written to the
    actual communication channel until the stream is flushed using stream_flush or
    there is no more room available in the stream buffer. If an error occurs, then
    it returns a negative error code. If the connection is closed it is considered
    an error condition.

    Unlike the stream_send_singles function, the size of the stream send buffer 
    must be at least as large as the number of 32-bit floats being sent.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each single-precision
    floating-point number when they are sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and 1 is returned. Some of the data may remain in the stream buffer and not be
    sent until the next time stream_flush is called or there is no more room available in
    the stream buffer. If an error occurs then the error code is returned and the stream
    should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 if the array is sent successfully.
    If the array could not be sent without blocking, then -QERR_WOULD_BLOCK is returned.
    If an error occurs then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = the array to be sent.
    num_elements = the number of elements in the array to send

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_single_array(t_stream stream, const t_single * elements, t_uint num_elements);

/*
    Name:   stream_send_double

    Description:

    This function writes a 64-bit double-precision floating-point number to the stream
    buffer. It attempts to store the double-precision floating-point number in the stream
    buffer. If there is enough room available in the stream buffer then it stores the data
    in the buffer and returns immediately. The data is not written to the actual communication
    channel until the stream is flushed using stream_flush or there is no more room available
    in the stream buffer. If an error occurs, then it returns a negative error code. If the
    connection is closed it is considered an error condition.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the double-precision floating-point
    number when they are sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and the total number of double-precision floating-point numbers sent is
    returned (1). Some of the data may remain in the stream buffer and not be sent until
    the next time stream_flush is called or there is no more room available in the stream
    buffer. If an error occurs then the error code is returned and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns the number of double-precision floating-point
    numbers sent successfully, which will be 1. If the double-precision floating-point
    number could not be sent without blocking, then -QERR_WOULD_BLOCK is returned. If an error
    occurs then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream = a client stream established using stream_connect or stream_accept.
    value  = the value to be sent.

    Return value:

    The number of double-precision floating-point numbers sent (1). If an error occurs then
    a negative error code is returned.
*/
EXTERN t_int
stream_send_double(t_stream stream, t_double value);

/*
    Name:   stream_send_doubles

    Description:

    This function writes an array of 64-bit double-precision floating-point numbers to
    the stream buffer. It attempts to store the double-precision floating-point numbers
    in the stream buffer. If there is enough room available in the stream buffer then
    it stores the data in the buffer and returns immediately. The data is not written
    to the actual communication channel until the stream is flushed using stream_flush
    or there is no more room available in the stream buffer. If an error occurs, then
    it returns a negative error code. If the connection is closed it is considered an
    error condition.

    Unlike the stream_send_double_array function, this function does not require that the
    stream send buffer be at least num_elements 64-bit floats in length. Hence, it
    allows smaller stream buffers to be used.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each double-precision
    floating-point number when they are sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to
    false (0), then this function may block attempting to flush the stream buffer. All
    the data will be consumed and the total number of double-precision floating-point
    numbers sent is returned. Some of the data may remain in the stream buffer and not
    be sent until the next time stream_flush is called or there is no more room available
    in the stream buffer. If an error occurs then the error code is returned and the
    stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to
    true (1), then this function does not block. It returns the number of double-precision
    floating-point numbers sent successfully, which will be between 1 and num_elements
    (unless num_elements is zero). If no double-precision floating-point numbers could
    be sent without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs
    then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = the array to be sent.
    num_elements = the number of elements in the array to send

    Return value:

    The number of double-precision floating-point numbers sent, which may be less than
    the number requested for non-blocking streams. If an error occurs then a negative
    error code is returned.
*/
EXTERN t_int
stream_send_doubles(t_stream stream, const t_double * elements, t_uint num_elements);

/*
    Name:   stream_send_double_array

    Description:

    This function writes an array of 64-bit double-precision floating-point numbers to
    the stream buffer. It attempts to store the double-precision floating-point numbers
    in the stream buffer. It differs from the stream_send_doubles function in that it
    treats the entire array as an atomic unit. It either writes all of the array or
    none of it. If there is enough room available in the stream buffer then it stores
    the data in the buffer and returns immediately. The data is not written to the
    actual communication channel until the stream is flushed using stream_flush or
    there is no more room available in the stream buffer. If an error occurs, then
    it returns a negative error code. If the connection is closed it is considered
    an error condition.

    Unlike the stream_send_doubles function, the size of the stream send buffer 
    must be at least as large as the number of 64-bit floats being sent.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each double-precision
    floating-point number when they are sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and 1 is returned. Some of the data may remain in the stream buffer and not be
    sent until the next time stream_flush is called or there is no more room available in
    the stream buffer. If an error occurs then the error code is returned and the stream
    should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 if the array is sent successfully.
    If the array could not be sent without blocking, then -QERR_WOULD_BLOCK is returned.
    If an error occurs then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    elements     = the array to be sent.
    num_elements = the number of elements in the array to send

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_double_array(t_stream stream, const t_double * elements, t_uint num_elements);

/*
    Name:   stream_send_unit

    Description:

    This function writes a "unit" to the stream buffer, where the size of a "unit" is
    determined by the unit_size parameter. It attempts to store the unit in the stream
    buffer. If there is enough room available in the stream buffer then it stores the
    data in the buffer and returns immediately. The data is not written to the actual
    communication channel until the stream is flushed using stream_flush or there is
    no more room available in the stream buffer. If an error occurs, then it returns
    a negative error code. If the connection is closed it is considered an error condition.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the unit when they are sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and the total number of units sent is returned (1). Some of the data may
    remain in the stream buffer and not be sent until the next time stream_flush is called or
    there is no more room available in the stream buffer. If an error occurs then the
    error code is returned and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns the number of units sent successfully,
    which will be 1. If the unit could not be sent without blocking, then -QERR_WOULD_BLOCK
    is returned. If an error occurs then the error code is returned and the stream should
    be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream    = a client stream established using stream_connect or stream_accept.
    unit      = a pointer to the value to be sent.
    unit_size = the size of the value

    Return value:

    The number of units sent (1). If an error occurs then a negative error
    code is returned.
*/
EXTERN t_int
stream_send_unit(t_stream stream, const void * unit, t_int unit_size);

/*
    Name:   stream_send_units

    Description:

    This function writes an array of "units" to the stream buffer, where the size
    of a "unit" is determined by the unit_size parameter. It attempts to store the
    units in the stream buffer. If there is enough room available in the stream buffer
    then it stores the data in the buffer and returns immediately. The data is not
    written to the actual communication channel until the stream is flushed using
    stream_flush or there is no more room available in the stream buffer. If an
    error occurs, then it returns a negative error code. If the connection is closed
    it is considered an error condition.

    Unlike the stream_send_unit_array function, this function does not require that the
    stream send buffer be at least num_units * unit_size bytes in length. Hence, it
    allows smaller stream buffers to be used.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each unit when they
    are sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and the total number of units sent is returned. Some of the data may
    remain in the stream buffer and not be sent until the next time stream_flush is called or
    there is no more room available in the stream buffer. If an error occurs then the
    error code is returned and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns the number of units sent successfully,
    which will be between 1 and num_units (unless num_units is zero). If no units
    could be sent without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs 
    then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    units        = the array to be sent.
    unit_size    = the size of one element in the array
    num_units    = the number of elements in the array to send

    Return value:

    The number of units sent, which may be less than the number requested for
    non-blocking streams. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_units(t_stream stream, const void * units, t_int unit_size, t_int num_units);

/*
    Name:   stream_send_unit_array

    Description:

    This function writes an array of "units" to the stream buffer, where the size
    of a "unit" is determined by the unit_size parameter. It attempts to store the
    "units" in the stream buffer. It differs from the stream_send_units function in
    that it treats the entire array as an atomic entity. It either writes all of the
    array or none of it. If there is enough room available in the stream buffer then
    it stores the data in the buffer and returns immediately. The data is not written
    to the actual communication channel until the stream is flushed using stream_flush
    or there is no more room available in the stream buffer. If an error occurs, then
    it returns a negative error code. If the connection is closed it is considered
    an error condition.

    Unlike the stream_send_units function, the size of the stream send buffer 
    must be at least as large as num_units * unit_size bytes being sent.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each unit when they are sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and 1 is returned. Some of the data may remain in the stream buffer and not be
    sent until the next time stream_flush is called or there is no more room available in
    the stream buffer. If an error occurs then the error code is returned and the stream
    should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 if the array is sent successfully.
    If the array could not be sent without blocking, then -QERR_WOULD_BLOCK is returned.
    If an error occurs then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    units        = the array to be sent.
    unit_size    = the size of one element in the array
    num_units    = the number of elements in the array to send

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_send_unit_array(t_stream stream, const void * units, t_int unit_size, t_int num_units);

/*
    Print Functions' Format Specifications

    The Quanser Stream API includes functions for printing to a stream. Each of these functions,
    defined below, takes a format string as one of their arguments. The format string determines
    the arguments that follow and the format and content of the output.

    Formatting specifications take the form:
        %<flags><field with>.<precision><modifier><argument type>[<code units>](<dimension>)

    flags:
        #       = use alternate form:
                    %b - prefixes output with 0b
                    %o - prefixes output with 0
                    %p - prefixes output with 0x
                    %x - prefixes output with 0x

        0       = use leading zeros instead of blanks to fill the field width

        -       = left justify the output within the field width

        <space> = use a leading blank to indicate a positive number

        +       = use a + to indicate a positive number

        '       = separate group of digits into thousands

    field width:
        the width in characters (not code units) of the resulting output. Specify * to indicate
        a variable field width, in which case the field width is specified via an argument.

    precision:
        the precision of the output. Specify * to indicate a variable precision, in which case
        the precision is specified via an argument. How the precision is interpreted depends on
        the argument type:

            %e, %f  = the number of fractional digits displayed
            %g      = the number of significant digits displayed
            %s      = the maximum number of characters (not code units, if <code units> argument specified) displayed

    modifier:
        hh      = integer argument is a char.
        h       = integer argument is a short. Character argument is a char. String argument is a multibyte string.
        I8      = integer argument is an 8-bit integer (t_int8). Character argument is a UTF-8 character (t_uint8). String argument is a UTF-8 string.
        I16     = integer argument is a 16-bit integer (t_int16). Character argument is a UTF-16 character (t_uint16). String argument is a UTF-16 string.
        I32     = integer argument is a 32-bit integer (t_int32). Floating-point argument is a 32-bit float (t_single). Character argument is a UTF-32 character (t_uint32). String argument is a UTF-32 string.
        I64     = integer argument is a 64-bit integer (t_int64). Floating-point argument is a 64-bit float (t_double).
        j       = integer argument is an intmax_t.
        l       = integer argument is a long. Floating-point argument is double. Character argument is a wchar_t. String argument is a wide string.
        ll      = integer argument is a long long (64-bit integer).
        L       = floating-point argument is a long double.
        q       = integer argument is a long long (64-bit integer).
        t       = integer argument is a ptrdiff_t.
        w       = Character argument is a wchar_t. String argument is a wide string.
        z       = integer argument is a size_t.

    argument type:
        b      = print the unsigned integer argument in binary, alternate form uses 0b as the prefix
        B      = print the unsigned integer argument in binary, alternate form uses 0B as the prefix
        c      = print a character
        C      = if MBCS defined:    print a wide character 
               = if UNICODE defined: print a multibyte character
        d      = print a signed integer value
        e      = print a floating-point value in exponential form e.g. 3.1415e2
        E      = print a floating-point value in exponential form using a capital E for the exponent e.g. 3.1415E2
        f      = print a floating-point value in standard form e.g. 314.15
        F      = print a floating-point value in standard form e.g. 314.15
        g      = print a floating-point value in general form (chooses between %f and %e appropriately)
        G      = print a floating-point value in general form (chooses between %F and %E appropriately)
        i      = print a signed integer value
        k      = callback (printed as a pointer).
        n      = return the number of code units, not characters, written so far to the output
        o      = print the unsigned integer argument in octal, alternate form uses a 0 prefix
        p      = print a pointer value, alternate form uses 0x prefix
        s      = print a string
        S      = if MBCS defined:    print a wide string
               = if UNICODE defined: print a multibyte string
        u      = print an unsigned integer value
        x      = print the unsigned integer argument in hexadecimal, alternate form uses a 0x prefix
        X      = print the unsigned integer argument in hexadecimal, alternate form uses a 0X prefix

    code units:
        the maximum number of code units. Specify * to indicate a variable number of code units, in 
        which case the number of code units is specified via an argument. How the number of code units
        is interpreted depends on the argument type:

            %c = the maximum number of code units in a character after conversion (defaults to 1)
            %s = the maximum number of code units in the string after conversion (defaults to the precision, if specified; otherwise unlimited)

    dimension:
        if specified, the argument is treated as an array with the specified dimension (length)
        Specify * to indicate a variable dimension, in which case the dimension is specified via an argument.
*/

/*
    Name:   stream_print_utf8_charsV

    Description:

    This function prints a formatted string of UTF-8 characters to the stream buffer. 
    The characters are converted according to the stream's character format prior to writing
    the character to the stream buffer. If no character format has been set, or the character
    format is STREAM_FORMAT_AUTO then the character format is set to UTF-8. This function
    attempts to store the converted characters in the stream buffer. If there is 
    enough room available in the stream buffer then it stores the data in the buffer and
    returns immediately. The data is not written to the actual communication channel until
    the stream is flushed using stream_flush or there is no more room available in the stream 
    buffer. If an error occurs, then it returns a negative error code. If the connection is closed
    it is considered an error condition.

    Unlike the stream_print_utf8_char_arrayV function, this function does not require that the
    stream send buffer be at least as large as the resulting formatted text in length. Hence,
    it allows smaller stream buffers to be used.

    Note that this function only writes complete characters to the stream. It will not write
    some but not all of the code units comprising a multi-unit character.

    Refer to the comments under the heading "Print Functions' Format Specification" for
    details on the format string.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the converted character if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and the total number of code units read from the caller's buffer is returned.
    Some of the data may remain in the stream buffer and not be sent until the next time 
    stream_flush is called or there is no more room available in the stream buffer. If an 
    error occurs then the error code is returned and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns the number of code units written to the
    caller's buffer and sent successfully after conversion. If the character could not be
    sent without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs then the
    error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    max_units      = the maximum number of code units (bytes) to write to the stream.
    fields_printed = a pointer to a variable of type t_int in which the number of fields 
                     successfully printed is stored.
    format         = a UTF-8 string containing the format specification, like the format
                     string in printf.
    arguments      = a va_list of arguments for substituting in the format string.

    Return value:

    The number of code units written to the caller's buffer and sent successfully 
    after conversion. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_print_utf8_charsV(t_stream stream, t_int max_units, t_int * fields_printed, const t_utf8_char * format, va_list arguments);

EXTERN t_int
stream_print_utf8_charsP(t_stream stream, t_int max_units, t_int * fields_printed, const t_utf8_char * format, pa_list arguments);

EXTERN t_int
string_print_utf8_charsV(t_utf8_char * string, t_int max_units, t_int * fields_printed, const t_utf8_char * format, va_list arguments);

/*
    Name:   stream_print_utf8_char_arrayV

    Description:

    This function prints a formatted string of UTF-8 characters to the stream buffer. 
    The characters are converted according to the stream's character format prior to writing
    the character to the stream buffer. If no character format has been set, or the character
    format is STREAM_FORMAT_AUTO then the character format is set to UTF-8. This function
    attempts to store the converted characters in the stream buffer. It differs from the 
    stream_print_utf8_charsV function in that it treats the entire array as an atomic unit.
    It either writes all of the formatted string or none of it. If there is enough room
    available in the stream buffer then it stores the data in the buffer and returns
    immediately. The data is not written to the actual communication channel until the
    stream is flushed using stream_flush or there is no more room available in the stream
    buffer. If an error occurs, then it returns a negative error code. If the connection
    is closed it is considered an error condition.

    Unlike the stream_print_utf8_charsV function, the size of the stream send buffer 
    must be at least as large as the resulting formatted text being sent.

    Note that this function only writes complete characters to the stream. It will not write
    some but not all of the code units comprising a multi-unit character.

    Refer to the comments under the heading "Print Functions' Format Specification" for
    details on the format string.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the converted character if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and 1 is returned. Some of the data may remain in the stream buffer and not be 
    sent until the next time stream_flush is called or there is no more room available in 
    the stream buffer. If an error occurs then the error code is returned and the stream 
    should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 if the array is sent successfully. If
    the array could not be sent without blocking, then -QERR_WOULD_BLOCK is returned. If
    an error occurs then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    max_units      = the maximum number of code units (bytes) to write to the stream.
    fields_printed = a pointer to a variable of type t_int in which the number of fields 
                     successfully printed is stored.
    format         = a UTF-8 string containing the format specification, like the format
                     string in printf.
    arguments      = a va_list of arguments for substituting in the format string.

    Return value:

    The number of code units written to the caller's buffer and sent successfully 
    after conversion. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_print_utf8_char_arrayV(t_stream stream, t_int max_units, t_int * fields_printed, const t_utf8_char * format, va_list arguments);

EXTERN t_int
stream_print_utf8_char_arrayP(t_stream stream, t_int max_units, t_int * fields_printed, const t_utf8_char * format, pa_list arguments);

/*
    Name:   stream_print_utf8_chars

    Description:

    This function prints a formatted string of UTF-8 characters to the stream buffer. 
    The characters are converted according to the stream's character format prior to writing
    the character to the stream buffer. If no character format has been set, or the character
    format is STREAM_FORMAT_AUTO then the character format is set to UTF-8. This function
    attempts to store the converted characters in the stream buffer. If there is 
    enough room available in the stream buffer then it stores the data in the buffer and
    returns immediately. The data is not written to the actual communication channel until
    the stream is flushed using stream_flush or there is no more room available in the stream 
    buffer. If an error occurs, then it returns a negative error code. If the connection is closed
    it is considered an error condition.

    Unlike the stream_print_utf8_char_array function, this function does not require that the
    stream send buffer be at least as large as the resulting formatted text in length. Hence,
    it allows smaller stream buffers to be used.

    Note that this function only writes complete characters to the stream. It will not write
    some but not all of the code units comprising a multi-unit character.

    Refer to the comments under the heading "Print Functions' Format Specification" for
    details on the format string.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the converted character if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and the total number of code units read from the caller's buffer is returned.
    Some of the data may remain in the stream buffer and not be sent until the next time 
    stream_flush is called or there is no more room available in the stream buffer. If an 
    error occurs then the error code is returned and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns the number of code units written to the
    caller's buffer and sent successfully after conversion. If the character could not be
    sent without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs then the
    error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    max_units      = the maximum number of code units (bytes) to write to the stream.
    fields_printed = a pointer to a variable of type t_int in which the number of fields 
                     successfully printed is stored.
    format         = a UTF-8 string containing the format specification, like the format
                     string in printf.
    ...            = subsequent arguments for substituting in the format string.

    Return value:

    The number of code units written to the caller's buffer and sent successfully 
    after conversion. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_print_utf8_chars(t_stream stream, t_int max_units, t_int * fields_printed, const t_utf8_char * format, ...);

EXTERN t_int
string_print_utf8_chars(t_utf8_char * string, t_int max_units, t_int * fields_printed, const t_utf8_char * format, ...);

EXTERN t_int
string_print_utf8_charsP(t_utf8_char * string, t_int max_units, t_int * fields_printed, const t_utf8_char * format, pa_list arguments);

EXTERN t_int
string_print_utf8_charsV(t_utf8_char * string, t_int max_units, t_int * fields_printed, const t_utf8_char * format, va_list arguments);

/*
    Name:   stream_print_utf8_char_array

    Description:

    This function prints a formatted string of UTF-8 characters to the stream buffer. 
    The characters are converted according to the stream's character format prior to writing
    the character to the stream buffer. If no character format has been set, or the character
    format is STREAM_FORMAT_AUTO then the character format is set to UTF-8. This function
    attempts to store the converted characters in the stream buffer. It differs from the 
    stream_print_utf8_chars function in that it treats the entire array as an atomic unit.
    It either writes all of the formatted string or none of it. If there is enough room
    available in the stream buffer then it stores the data in the buffer and returns
    immediately. The data is not written to the actual communication channel until the
    stream is flushed using stream_flush or there is no more room available in the stream
    buffer. If an error occurs, then it returns a negative error code. If the connection
    is closed it is considered an error condition.

    Unlike the stream_print_utf8_chars function, the size of the stream send buffer 
    must be at least as large as the resulting formatted text being sent.

    Note that this function only writes complete characters to the stream. It will not write
    some but not all of the code units comprising a multi-unit character.

    Refer to the comments under the heading "Print Functions' Format Specification" for
    details on the format string.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the converted character if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer. All the data will be
    consumed and 1 is returned. Some of the data may remain in the stream buffer and not be 
    sent until the next time stream_flush is called or there is no more room available in 
    the stream buffer. If an error occurs then the error code is returned and the stream 
    should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 if the array is sent successfully. If
    the array could not be sent without blocking, then -QERR_WOULD_BLOCK is returned. If
    an error occurs then the error code is returned and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    max_units      = the maximum number of code units (bytes) to write to the stream.
    fields_printed = a pointer to a variable of type t_int in which the number of fields 
                     successfully printed is stored.
    format         = a UTF-8 string containing the format specification, like the format
                     string in printf.
    ...            = subsequent arguments for substituting in the format string.

    Return value:

    The number of code units written to the caller's buffer and sent successfully 
    after conversion. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_print_utf8_char_array(t_stream stream, t_int max_units, t_int * fields_printed, const t_utf8_char * format, ...);

/*
    Scan Functions' Format Specifications

    The Quanser Stream API includes functions for scanning formatted text from a stream. 
    Each of these functions, defined below, takes a format string as one of their arguments. 
    The format string determines the arguments that follow and the format and content of the
    output.

    Formatting specifications take the form:
        %<flags><.><field with><modifier><argument type>[<code units>](<dimension>)

    flags:
        *       = suppress assignment of the scanned value to an argument

        #       = use alternate form:
                    %b - recognizes optional 0b prefix
                    %d - recognizes optional prefixes 0b, 0x or 0
                    %i - recognizes optional prefixes 0b, 0x or 0 (%i recognizes these prefixes even without alternate form modifier)
                    %u - recognizes optional prefixes 0b, 0x or 0
                    %o - recognizes optional 0  prefix
                    %p - recognizes optional 0x prefix
                    %x - recognizes optional 0x prefix

        '       = recognizes when groups of digits are separated into thousands

    field width:
        the maximum width in characters (not code units) of the input. Specify .* to indicate
        a variable field width, in which case the field width is specified via an argument. The
        '.' is required to differentiate between assignment suppression and a variable field width.

    modifier:
        hh      = integer argument is a char.
        h       = integer argument is a short. Character argument is a char. String argument is a multibyte string.
        I8      = integer argument is an 8-bit integer (t_int8). Character argument is a UTF-8 character (t_uint8). String argument is a UTF-8 string.
        I16     = integer argument is a 16-bit integer (t_int16). Character argument is a UTF-16 character (t_uint16). String argument is a UTF-16 string.
        I32     = integer argument is a 32-bit integer (t_int32). Floating-point argument is a 32-bit float (t_single). Character argument is a UTF-32 character (t_uint32). String argument is a UTF-32 string.
        I64     = integer argument is a 64-bit integer (t_int64). Floating-point argument is a 64-bit float (t_double).
        j       = integer argument is an intmax_t.
        l       = integer argument is a long. Floating-point argument is double. Character argument is a wchar_t. String argument is a wide string.
        ll      = integer argument is a long long (64-bit integer).
        L       = floating-point argument is a long double.
        q       = integer argument is a long long (64-bit integer).
        t       = integer argument is a ptrdiff_t.
        w       = Character argument is a wchar_t. String argument is a wide string.
        z       = integer argument is a size_t.

    argument type:
        b      = print the unsigned integer argument in binary, alternate form uses 0b as the prefix
        B      = print the unsigned integer argument in binary, alternate form uses 0B as the prefix
        c      = print a character
        C      = if MBCS defined:    print a wide character 
               = if UNICODE defined: print a multibyte character
        d      = print a signed integer value
        e      = print a floating-point value in exponential form e.g. 3.1415e2
        E      = print a floating-point value in exponential form using a capital E for the exponent e.g. 3.1415E2
        f      = print a floating-point value in standard form e.g. 314.15
        F      = print a floating-point value in standard form e.g. 314.15
        g      = print a floating-point value in general form (chooses between %f and %e appropriately)
        G      = print a floating-point value in general form (chooses between %F and %E appropriately)
        i      = print a signed integer value
        k      = callback (printed as a pointer).
        n      = return the number of code units, not characters, written so far to the output
        o      = print the unsigned integer argument in octal, alternate form uses a 0 prefix
        p      = print a pointer value, alternate form uses 0x prefix
        s      = print a string
        S      = if MBCS defined:    print a wide string
               = if UNICODE defined: print a multibyte string
        u      = print an unsigned integer value
        x      = print the unsigned integer argument in hexadecimal, alternate form uses a 0x prefix
        X      = print the unsigned integer argument in hexadecimal, alternate form uses a 0X prefix

    code units:
        the maximum number of code units. Specify * to indicate a variable number of code units, in which case the
        number of code units is specified via an argument. How the number of code units is interpreted depends
        on the argument type:

            %c = the maximum number of code units in a character after conversion (defaults to 1)
            %s = the maximum number of code units in the string after conversion (defaults to the field width, if specified; otherwise unlimited)

    dimension:
        if specified, the argument is treated as an array with the specified dimension (length). 
        Specify * to indicate a variable dimension, in which case the dimension is specified via an argument.
*/

/*
    Name:   stream_scan_utf8_charsV

    Description:

    This function reads a formatted string of UTF-8 characters from the stream. 
    The characters are converted according to the stream's character format prior to parsing
    the text according to the format string. If no character format has been set, or the character
    format is STREAM_FORMAT_AUTO then the character format is set to UTF-8. Parsed
    values are stored in the arguments, which must be pointers to the appropriate datatypes. 
    If an error occurs, then it returns a negative error code. 
    
    If the connection is closed before any characters have been read from the stream, then
    this function returns 0. Otherwise it returns the number of code units read in the
    process of scanning the input stream.

    Unlike the stream_scan_utf8_char_arrayV function, this function does not require that the
    stream receive buffer be at least as large as the formatted text in length. Hence,
    it allows smaller stream buffers to be used.

    Note that this function only reads complete characters from the stream. It will not read
    some but not all of the code units comprising a multi-unit character.

    Refer to the comments under the heading "Scan Functions' Format Specification" for
    details on the format string.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the characters read if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to read from the stream. In particular, note
    that it may have to "read ahead" in order to parse the input stream. For example, if
    the format string is "%d" and the input stream is "2", this function will have to read
    another character after the '2' to see if it is another digit in the number or a
    delimiter.
    
    If the input does not match the format string then -QERR_MISMATCHED_CHARACTER is
    returned. In this case, the stream is advanced to the end of the last character or
    field successfully scanned. This behaviour differs from the stream_scan_utf8_char_arrayV
    function, which does not advance the stream unless the entire format string is
    matched by the input. The stream may be rescanned. However, scanning will begin at
    the first character after the last character or field successfuly scanned previously.
    
    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns -QERR_WOULD_BLOCK if reading from the
    stream would block. In this case, the stream is advanced to the end of the last character or
    field successfully scanned without blocking. This behaviour differs from the stream_scan_utf8_char_arrayV
    function, which does not advance the stream unless the entire format string is
    matched by the input without blocking. The stream may be rescanned. However, scanning
    will begin at the first character after the last character or field successfuly scanned
    previously.

    If an error occurs reading from the stream then the error code is returned
    and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    max_units      = the maximum number of code units (bytes) to read from the stream.
    fields_scanned = a pointer to a variable of type t_int in which the number of fields 
                     successfully scanned is stored.
    format         = a UTF-8 string containing the format specification, like the format
                     string in scanf.
    arguments      = a va_list of arguments to receive the values parsed from the input 
                     stream.

    Return value:

    The number of code units read from the input stream. If  the input stream does not 
    match the format specification, then -QERR_MISMATCHED_CHARACTER is returned. If an
    error occurs reading from the stream then a negative error code is returned.
*/
EXTERN t_int
stream_scan_utf8_charsV(t_stream stream, t_int max_units, t_int * fields_scanned, const t_utf8_char * format, va_list arguments);

EXTERN t_int
stream_scan_utf8_charsP(t_stream stream, t_int max_units, t_int * fields_scanned, const t_utf8_char * format, pa_list arguments);

EXTERN t_int
string_scan_utf8_charsV(const t_utf8_char * string, t_int max_units, t_int * fields, const t_utf8_char * format, va_list arguments);

EXTERN t_int
string_scan_utf8_charsP(const t_utf8_char * string, t_int max_units, t_int * fields, const t_utf8_char * format, pa_list arguments);

/*
    Name:   stream_scan_utf8_chars

    Description:

    This function reads a formatted string of UTF-8 characters from the stream. 
    The characters are converted according to the stream's character format prior to parsing
    the text according to the format string. If no character format has been set, or the character
    format is STREAM_FORMAT_AUTO then the character format is set to UTF-8. Parsed
    values are stored in the arguments, which must be pointers to the appropriate datatypes. 
    If an error occurs, then it returns a negative error code. 
    
    If the connection is closed before any characters have been read from the stream, then
    this function returns 0. Otherwise it returns the number of code units read in the
    process of scanning the input stream.

    Unlike the stream_scan_utf8_char_array function, this function does not require that the
    stream receive buffer be at least as large as the formatted text in length. Hence,
    it allows smaller stream buffers to be used.

    Note that this function only reads complete characters from the stream. It will not read
    some but not all of the code units comprising a multi-unit character.

    Refer to the comments under the heading "Scan Functions' Format Specification" for
    details on the format string.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the characters read if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to read from the stream. In particular, note
    that it may have to "read ahead" in order to parse the input stream. For example, if
    the format string is "%d" and the input stream is "2", this function will have to read
    another character after the '2' to see if it is another digit in the number or a
    delimiter.
    
    If the input does not match the format string then -QERR_MISMATCHED_CHARACTER is
    returned. In this case, the stream is advanced to the end of the last character or
    field successfully scanned. This behaviour differs from the stream_scan_utf8_char_array
    function, which does not advance the stream unless the entire format string is
    matched by the input. The stream may be rescanned. However, scanning will begin at
    the first character after the last character or field successfuly scanned previously.
    
    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns -QERR_WOULD_BLOCK if reading from the
    stream would block. In this case, the stream is advanced to the end of the last character or
    field successfully scanned without blocking. This behaviour differs from the stream_scan_utf8_char_array
    function, which does not advance the stream unless the entire format string is
    matched by the input without blocking. The stream may be rescanned. However, scanning
    will begin at the first character after the last character or field successfuly scanned
    previously.

    If an error occurs reading from the stream then the error code is returned
    and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    max_units      = the maximum number of code units (bytes) to read from the stream.
    fields_scanned = a pointer to a variable of type t_int in which the number of fields 
                     successfully scanned is stored.
    format         = a UTF-8 string containing the format specification, like the format
                     string in scanf.
    ...            = subsequent arguments to receive the values parsed from the input 
                     stream.

    Return value:

    The number of code units read from the input stream. If the input stream does not 
    match the format specification, then -QERR_MISMATCHED_CHARACTER is returned. If an
    error occurs reading from the stream then a negative error code is returned.
*/
EXTERN t_int
stream_scan_utf8_chars(t_stream stream, t_int max_units, t_int * fields_scanned, const t_utf8_char * format, ...);

EXTERN t_int
string_scan_utf8_chars(const t_utf8_char * string, t_int max_units, t_int * fields_scaned, const t_utf8_char * format, ...);

/*
    Name:   stream_scan_utf8_char_arrayV

    Description:

    This function reads a formatted string of UTF-8 characters from the stream as an atomic unit. 
    The characters are converted according to the stream's character format prior to parsing
    the text according to the format string. If no character format has been set, or the character
    format is STREAM_FORMAT_AUTO then the character format is set to UTF-8. Parsed
    values are stored in the arguments, which must be pointers to the appropriate datatypes. 
    If an error occurs, then it returns a negative error code. 
    
    If the connection is closed before any characters have been read from the stream, then
    this function returns 0. Otherwise it returns the number of code units read in the
    process of scanning the input stream.

    Unlike the stream_scan_utf8_charsV function, this function requires that the
    stream receive buffer be at least as large as the entire formatted text in length.
    However, it has the benefit that the entire text may be rescanned if a formatting
    mismatch occurs or the stream would block.

    Note that this function only reads complete characters from the stream. It will not read
    some but not all of the code units comprising a multi-unit character.

    Refer to the comments under the heading "Scan Functions' Format Specification" for
    details on the format string.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the characters read if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to read from the stream. In particular, note
    that it may have to "read ahead" in order to parse the input stream. For example, if
    the format string is "%d" and the input stream is "2", this function will have to read
    another character after the '2' to see if it is another digit in the number or a
    delimiter.
    
    If the input does not match the format string then -QERR_MISMATCHED_CHARACTER is
    returned. In this case, the stream is not advanced at all. This behaviour differs 
    from the stream_scan_utf8_charsV function, which advances the stream to the end of
    the last character or field successfully scanned. The stream may be rescanned.
    Scanning will begin at the same place in the stream as the previous call, so the
    entire formatted text may be rescanned, even if some fields were matched in the
    previous call. 
    
    If the input matches the entire format string then the stream is advanced. The next
    call will read subsequent characters from the stream.
    
    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns -QERR_WOULD_BLOCK if reading from the
    stream would block. In this case, the stream is not advanced at all. This behaviour 
    differs from the stream_scan_utf8_charsV function, which does advances the stream to the
    end of the last character or field successfully scanned. The stream may be rescanned.
    Scanning will begin at the same place in the stream as the previous call, so the
    entire formatted text may be rescanned, even if some fields were matched in the
    previous call.

    If an error occurs reading from the stream then the error code is returned
    and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    max_units      = the maximum number of code units (bytes) to read from the stream.
    fields_scanned = a pointer to a variable of type t_int in which the number of fields 
                     successfully scanned is stored.
    format         = a UTF-8 string containing the format specification, like the format
                     string in scanf.
    arguments      = a va_list of arguments to receive the values parsed from the input 
                     stream.

    Return value:

    The number of code units read from the input stream. If  the input stream does not 
    match the format specification, then -QERR_MISMATCHED_CHARACTER is returned. If an
    error occurs reading from the stream then a negative error code is returned.
*/
EXTERN t_int
stream_scan_utf8_char_arrayV(t_stream stream, t_int max_units, t_int * fields_scanned, const t_utf8_char * format, va_list arguments);

EXTERN t_int
stream_scan_utf8_char_arrayP(t_stream stream, t_int max_units, t_int * fields_scanned, const t_utf8_char * format, pa_list arguments);

/*
    Name:   stream_scan_utf8_char_array

    Description:

    This function reads a formatted string of UTF-8 characters from the stream as an atomic unit. 
    The characters are converted according to the stream's character format prior to parsing
    the text according to the format string. If no character format has been set, or the character
    format is STREAM_FORMAT_AUTO then the character format is set to UTF-8. Parsed
    values are stored in the arguments, which must be pointers to the appropriate datatypes. 
    If an error occurs, then it returns a negative error code. 
    
    If the connection is closed before any characters have been read from the stream, then
    this function returns 0. Otherwise it returns the number of code units read in the
    process of scanning the input stream.

    Unlike the stream_scan_utf8_chars function, this function requires that the
    stream receive buffer be at least as large as the entire formatted text in length.
    However, it has the benefit that the entire text may be rescanned if a formatting
    mismatch occurs or the stream would block.

    Note that this function only reads complete characters from the stream. It will not read
    some but not all of the code units comprising a multi-unit character.

    Refer to the comments under the heading "Scan Functions' Format Specification" for
    details on the format string.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the characters read if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to read from the stream. In particular, note
    that it may have to "read ahead" in order to parse the input stream. For example, if
    the format string is "%d" and the input stream is "2", this function will have to read
    another character after the '2' to see if it is another digit in the number or a
    delimiter.
    
    If the input does not match the format string then -QERR_MISMATCHED_CHARACTER is
    returned. In this case, the stream is not advanced at all. This behaviour differs 
    from the stream_scan_utf8_chars function, which advances the stream to the end of
    the last character or field successfully scanned. The stream may be rescanned.
    Scanning will begin at the same place in the stream as the previous call, so the
    entire formatted text may be rescanned, even if some fields were matched in the
    previous call. 
    
    If the input matches the entire format string then the stream is advanced. The next
    call will read subsequent characters from the stream.
    
    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns -QERR_WOULD_BLOCK if reading from the
    stream would block. In this case, the stream is not advanced at all. This behaviour 
    differs from the stream_scan_utf8_chars function, which does advances the stream to the
    end of the last character or field successfully scanned. The stream may be rescanned.
    Scanning will begin at the same place in the stream as the previous call, so the
    entire formatted text may be rescanned, even if some fields were matched in the
    previous call.

    If an error occurs reading from the stream then the error code is returned
    and the stream should be closed.

    This function does not support two threads sending or flushing data at the same time.
    However, data may be sent or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    max_units      = the maximum number of code units (bytes) to read from the stream.
    fields_scanned = a pointer to a variable of type t_int in which the number of fields 
                     successfully scanned is stored.
    format         = a UTF-8 string containing the format specification, like the format
                     string in scanf.
    ...            = subsequent arguments to receive the values parsed from the input 
                     stream.

    Return value:

    The number of code units read from the input stream. If  the input stream does not 
    match the format specification, then -QERR_MISMATCHED_CHARACTER is returned. If an
    error occurs reading from the stream then a negative error code is returned.
*/
EXTERN t_int
stream_scan_utf8_char_array(t_stream stream, t_int max_units, t_int * fields_scanned, const t_utf8_char * format, ...);

/*
    Name:   stream_flush

    Description:

    This function flushes the stream buffer. It attempts to send the contents of the buffer
    over the communication channel. If an error occurs, then it returns a negative error code.
    If the connection is closed it is considered an error condition.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data in the buffer is sent.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It attempts to send all the data remaining in the
    stream buffer. However, if this operation would block then it returns -QERR_WOULD_BLOCK,
    even if it has already sent some of the data. In this case, the stream_poll function may
    be used with the STREAM_POLL_FLUSH flag to determine when at least one more byte may be
    flushed.

    This function does not support two threads calling stream_send or stream_flush at the same
    time. However, stream_send or stream_flush may be called by another thread at the same time
    as stream_receive.

    Parameters:

    stream = a client stream established using stream_connect or stream_accept.

    Return value:

    Returns 0 if the buffer is flushed. If an error occurs then a negative error code is
    returned.
*/
EXTERN t_error
stream_flush(t_stream stream);

/*
    Name:   stream_peek_begin

    Description:

    This function prepares the t_stream_peek_state to begin peeking for data. "Peeking" involves
    reading ahead without removing the data from the stream buffer. It is particularly useful for
    parsing the input from the stream because it allows the next bytes to be tested without
    preventing the user from receiving those bytes later. It may also be used to read a sequence
    of data types as a single atomic unit, much like the xxxx_array functions. The "peek state" 
    is used to keep track of how far ahead the lookahead operations have reached so far.
 
    The stream_peek_end function must be called to end the peek operation. Failure to do so
    may potentially cause deadlock the next time stream_peek_begin is called.

    As an example of using the peek capabilities to read a sequence of data types as a single
    atomic unit using non-blocking I/O:

        t_stream_peek_state state;
        t_int int_value;
        t_double double_value;

        result = stream_peek_begin(stream, &state, 0);
        if (result > 0) {
            result = stream_peek_int(stream, &state, &int_value);
            if (result > 0) {
                result = stream_peek_double(stream, &state, &double_value);
            }
            
            result = stream_peek_end(stream, &state, result);
        }

    In this example, the code peeks ahead in the input stream for an integer and a double.
    If at any time the operation would block, then -QERR_WOULD_BLOCK is returned. The
    stream_peek_end call never advances the stream pointer because the result is less than 1
    and the data is not removed from the input stream. Hence, the next time the code is called
    the same data is read again. The stream_peek_end also returns the same result so that the
    -QERR_WOULD_BLOCK error is returned.

    However, if the stream_peek_int and stream_peek_double succeed then stream_peek_end
    advances the stream pointer and returns 1. Hence, the data is removed from the input
    stream and subsequent reads/peeks from the stream will return new data.

    Thus, the integer and double are read as one atomic unit from the input stream, because
    if not all the data is available, then nothing is removed from the input stream. Only
    if both quantities are read successfully is the data actually removed from the input
    stream.

    Note that the stream_peek_xxxx functions may read data from the underlying communication
    channel, but the contents of the stream buffer are never overwritten until stream_peek_end
    is called to indicate that the peeked data may be removed from the input stream.

    This function assumes that the stream is valid. If the prefetch is zero then it does
    not block because it does not access the underlying communication channel.

    However, if the prefetch is non-zero then it may access the underlying communication
    channel. Normally, the stream will read as much as it can on any receive operation
    and try to fill the stream's receive buffer. Hence, no prefetching of data is necessary
    because the stream will do so automatically on any receive operation. However, if
    read-ahead has been disabled by setting the STREAM_PROPERTY_NO_READ_AHEAD property then
    the stream only reads as much data as is required to satisfy the receive operation
    requested. Some protocols, like I2C, enforce this property and do not allow read-ahead.
    The prefetch parameter of stream_peek_begin causes the stream to attempt to ensure that
    at least "prefetch" bytes are present in the stream receive buffer before the rest of
    the stream_peek_xxx operations are performed, thereby reducing the number of calls to
    the underlying stream to receive data when read-ahead has been disabled.

    If prefetch is not required (because read-ahead is enabled on the stream - the default
    situation) then the prefetch parameter may be zero. If prefetch is desired, then it
    should be set to *no more* than the number of bytes expected to be peeked in subsequent
    stream_peek_xxx calls.

    For example, suppose the following binary structure is being received:

        struct {
            t_int8 temperature;
            t_uint8 status;
            t_int16 gyro_rates[3];
        }

    This structure is 1 + 1 + 3*2 = 8 bytes long. Hence, the code to read this structure
    as one atomic unit, while accounting for potential byte swapping due to byte order
    differences between server and client would be:

        t_stream_peek_state peek_state;
        t_error result;
        t_int8 temperature;
        t_uint8 status;
        t_int16 gyro_rates[3];

        result = stream_peek_begin(stream, &peek_state,
            sizeof(t_int8) + sizeof(t_uint8) + 3 * sizeof(t_int16)); // prefetch 8 bytes
        if (result > 0)
        {
            result = stream_peek_byte(stream, &temperature);
            if (result > 0)
            {
                result = stream_peek_byte(stream, (t_byte *) &status);
                if (result > 0)
                    result = stream_peek_short_array(stream, &gyro_rates[0]);
            }
            result = stream_peek_end(stream, &peek_state, result);
        }

    Parameters:

    stream    = a client stream established using stream_connect or stream_accept.
    state     = the "peek state" to initialize.
    prefetch  = number of bytes to prefetch even if read-ahead is disabled.

    Return value:

    This function returns an error if one of the parameters is invalid or the
    stream is shutdown or invalid. It the stream is closed gracefully during a
    prefetch operation and there are not enough bytes left in the receive buffer
    for the prefetch specified then it will return zero. It returns 1 on success.
*/
EXTERN t_error
stream_peek_begin(t_stream stream, t_stream_peek_state * state, t_int prefetch);

/*
    Name:   stream_peek_byte

    Description:

    This function peeks ahead in the input stream to read a byte from the stream.

    The stream_peek_begin function must be called to initialize the peek state. No data
    peeked will be removed from the input stream until stream_peek_end is called. If
    stream_peek_end is not called before the next stream_peek_begin then the peeked
    data is read again.

    See the stream_peek_begin function for more information and an example.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is peeked. Blocking may occur because 
    data may be read from the underlying communication channel. However, the contents of 
    the stream buffer are never overwritten until stream_peek_end is called to indicate 
    that the peeked data may be removed from the input stream.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If no byte is available then it returns 
    -QERR_WOULD_BLOCK. In this case, the stream_poll function may be used with the 
    STREAM_POLL_RECEIVE flag to determine when data becomes available. Otherwise it 
    returns 1.

    If the connection has been closed gracefully then it returns 0. If an error occurs, then
    it returns a negative error code.

    This function does not support two threads peeking data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    peeked.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream      = a client stream established using stream_connect or stream_accept.
    state       = the "peek state" initialized using stream_peek_begin
    value       = a pointer to a t_byte variable in which the peeked data will be stored.

    Return value:

    Returns 1 if the data is successfully peeked from the stream. If the connection has 
    been closed gracefully then 0 is returned. If an error occurs then a negative error 
    code is returned.
*/
EXTERN t_int
stream_peek_byte(t_stream stream, t_stream_peek_state * state, t_byte * value);

/*
    Name:   stream_peek_byte_array

    Description:

    This function peeks ahead in the input stream to read an array of bytes from a client 
    stream. It either peeks all of the array or none of it.

    The stream_peek_begin function must be called to initialize the peek state. No data
    peeked will be removed from the input stream until stream_peek_end is called. If
    stream_peek_end is not called before the next stream_peek_begin then the peeked
    data is read again.

    See the stream_peek_begin function for more information and an example.

    The size of the stream receive buffer must be at least num_elements bytes in 
    length.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is peeked. Blocking may occur because 
    data may be read from the underlying communication channel. However, the contents of 
    the stream buffer are never overwritten until stream_peek_end is called to indicate 
    that the peeked data may be removed from the input stream.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    the entire array then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to peek than the size of the entire array. Otherwise it returns 1. Once
    there are fewer bytes left to peek than the size of the entire array then it will
    return 0 to indicate the connection has been closed. Use stream_peek to peek any
    remaining bytes if required. If an error occurs, then it returns a negative error code.

    This function does not support two threads peeking data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    peeked.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    state        = the "peek state" initialized using stream_peek_begin
    buffer       = a pointer to a t_byte array in which the peeked data will be stored.
    num_elements = the number of elements to peek, and available in the elements array.

    Return value:

    Returns 1 on success. If not enough data is available and the connection has been
    closed gracefully then 0 is returned. If an error occurs then a negative error code
    is returned.
*/
EXTERN t_int
stream_peek_byte_array(t_stream stream, t_stream_peek_state * state, t_byte * buffer, t_int num_elements);

/*
    Name:   stream_peek_short

    Description:

    This function peeks ahead in the input stream to read a short (16-bit) integer from the
    stream. If the stream has been configured to swap bytes using stream_set_swap_bytes then 
    this function will swap the order of the bytes that it peeks before storing them in the 
    given buffer.

    The stream_peek_begin function must be called to initialize the peek state. No data
    peeked will be removed from the input stream until stream_peek_end is called. If
    stream_peek_end is not called before the next stream_peek_begin then the peeked
    data is read again.

    See the stream_peek_begin function for more information and an example.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is peeked. Blocking may occur because 
    data may be read from the underlying communication channel. However, the contents of 
    the stream buffer are never overwritten until stream_peek_end is called to indicate 
    that the peeked data may be removed from the input stream.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of a
    short integer then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll function 
    may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes available.
    Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to peek than the size of a short integer. Otherwise it returns the number of short
    integers peeked before the connection closed. Once there are fewer bytes left to peek than 
    the size of a short integer then it will return 0 to indicate the connection has been closed.
    Use stream_peek to peek any remaining bytes if required. If an error occurs, then
    it returns a negative error code.

    This function does not support two threads peeking data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    peeked.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream      = a client stream established using stream_connect or stream_accept.
    state       = the "peek state" initialized using stream_peek_begin
    value       = a pointer to a variable in which the peeked data will be stored.

    Return value:

    Returns 1 if the data is successfully peeked from the stream. If fewer bytes than the size of 
    a short integer are available and the connection has been closed gracefully then 0 is returned. 
    If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_peek_short(t_stream stream, t_stream_peek_state * state, t_short * value);

/*
    Name:   stream_peek_short_array

    Description:

    This function peeks ahead in the input stream to read an array of short (16-bit) integers
    from a client stream. It either peeks all of the array or none of it. If the stream has 
    been configured to swap bytes using stream_set_swap_bytes then this function will 
    swap the order of the bytes within each element that it peeks before storing them in the 
    given buffer.

    The stream_peek_begin function must be called to initialize the peek state. No data
    peeked will be removed from the input stream until stream_peek_end is called. If
    stream_peek_end is not called before the next stream_peek_begin then the peeked
    data is read again.

    See the stream_peek_begin function for more information and an example.

    The size of the stream receive buffer must be at least num_elements * sizeof(t_short)
    bytes in length.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is peeked. Blocking may occur because 
    data may be read from the underlying communication channel. However, the contents of 
    the stream buffer are never overwritten until stream_peek_end is called to indicate 
    that the peeked data may be removed from the input stream.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    the entire array then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to peek than the size of the entire array. Otherwise it returns 1. Once
    there are fewer bytes left to peek than the size of the entire array then it will
    return 0 to indicate the connection has been closed. Use stream_peek to peek any
    remaining bytes if required. If an error occurs, then it returns a negative error code.

    This function does not support two threads peeking data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    peeked.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    state        = the "peek state" initialized using stream_peek_begin
    buffer       = a pointer to an array of short integers in which the peeked data 
                   will be stored. It must be at least num_elements in length.
    num_elements = the number of elements to peek

    Return value:

    Returns 1 on success. If not enough data is available and the connection has been
    closed gracefully then 0 is returned. If an error occurs then a negative error code
    is returned.
*/
EXTERN t_int
stream_peek_short_array(t_stream stream, t_stream_peek_state * state, t_short * buffer, t_int num_elements);

/*
    Name:   stream_peek_int

    Description:

    This function peeks ahead in the input stream to read a 32-bit integer from the
    stream. If the stream has been configured to swap bytes using stream_set_swap_bytes then 
    this function will swap the order of the bytes that it peeks before storing them in the 
    given buffer.

    The stream_peek_begin function must be called to initialize the peek state. No data
    peeked will be removed from the input stream until stream_peek_end is called. If
    stream_peek_end is not called before the next stream_peek_begin then the peeked
    data is read again.

    See the stream_peek_begin function for more information and an example.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is peeked. Blocking may occur because 
    data may be read from the underlying communication channel. However, the contents of 
    the stream buffer are never overwritten until stream_peek_end is called to indicate 
    that the peeked data may be removed from the input stream.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of an
    integer then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll function 
    may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes available.
    Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to peek than the size of an integer. Otherwise it returns the number of 32-bit
    integers peeked before the connection closed. Once there are fewer bytes left to peek than 
    the size of an integer then it will return 0 to indicate the connection has been closed.
    Use stream_peek to peek any remaining bytes if required. If an error occurs, then
    it returns a negative error code.

    This function does not support two threads peeking data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    peeked.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream      = a client stream established using stream_connect or stream_accept.
    state       = the "peek state" initialized using stream_peek_begin
    value       = a pointer to a variable in which the peeked data will be stored.

    Return value:

    Returns 1 if the data is successfully peeked from the stream. If fewer bytes than the size of 
    an integer are available and the connection has been closed gracefully then 0 is returned. 
    If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_peek_int(t_stream stream, t_stream_peek_state * state, t_int * value);

/*
    Name:   stream_peek_int_array

    Description:

    This function peeks ahead in the input stream to read an array of 32-bit integers
    from a client stream. It either peeks all of the array or none of it. If the stream has 
    been configured to swap bytes using stream_set_swap_bytes then this function will 
    swap the order of the bytes within each element that it peeks before storing them in the 
    given buffer.

    The stream_peek_begin function must be called to initialize the peek state. No data
    peeked will be removed from the input stream until stream_peek_end is called. If
    stream_peek_end is not called before the next stream_peek_begin then the peeked
    data is read again.

    See the stream_peek_begin function for more information and an example.

    The size of the stream receive buffer must be at least num_elements * sizeof(t_int)
    bytes in length.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is peeked. Blocking may occur because 
    data may be read from the underlying communication channel. However, the contents of 
    the stream buffer are never overwritten until stream_peek_end is called to indicate 
    that the peeked data may be removed from the input stream.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    the entire array then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to peek than the size of the entire array. Otherwise it returns 1. Once
    there are fewer bytes left to peek than the size of the entire array then it will
    return 0 to indicate the connection has been closed. Use stream_peek to peek any
    remaining bytes if required. If an error occurs, then it returns a negative error code.

    This function does not support two threads peeking data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    peeked.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    state        = the "peek state" initialized using stream_peek_begin
    buffer       = a pointer to an array of integers in which the peeked data 
                   will be stored. It must be at least num_elements in length.
    num_elements = the number of elements to peek

    Return value:

    Returns 1 on success. If not enough data is available and the connection has been
    closed gracefully then 0 is returned. If an error occurs then a negative error code
    is returned.
*/
EXTERN t_int
stream_peek_int_array(t_stream stream, t_stream_peek_state * state, t_int * buffer, t_int num_elements);

/*
    Name:   stream_peek_long

    Description:

    This function peeks ahead in the input stream to read a long (64-bit) integer from the
    stream. If the stream has been configured to swap bytes using stream_set_swap_bytes then 
    this function will swap the order of the bytes that it peeks before storing them in the 
    given buffer.

    The stream_peek_begin function must be called to initialize the peek state. No data
    peeked will be removed from the input stream until stream_peek_end is called. If
    stream_peek_end is not called before the next stream_peek_begin then the peeked
    data is read again.

    See the stream_peek_begin function for more information and an example.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is peeked. Blocking may occur because 
    data may be read from the underlying communication channel. However, the contents of 
    the stream buffer are never overwritten until stream_peek_end is called to indicate 
    that the peeked data may be removed from the input stream.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of a
    long integer then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll function 
    may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes available.
    Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to peek than the size of a long integer. Otherwise it returns the number of long
    integers peeked before the connection closed. Once there are fewer bytes left to peek than 
    the size of a long integer then it will return 0 to indicate the connection has been closed.
    Use stream_peek to peek any remaining bytes if required. If an error occurs, then
    it returns a negative error code.

    This function does not support two threads peeking data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    peeked.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream      = a client stream established using stream_connect or stream_accept.
    state       = the "peek state" initialized using stream_peek_begin
    value       = a pointer to a variable in which the peeked data will be stored.

    Return value:

    Returns 1 if the data is successfully peeked from the stream. If fewer bytes than the size of 
    a long integer are available and the connection has been closed gracefully then 0 is returned. 
    If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_peek_long(t_stream stream, t_stream_peek_state * state, t_long * value);

/*
    Name:   stream_peek_long_array

    Description:

    This function peeks ahead in the input stream to read an array of long (64-bit) integers
    from a client stream. It either peeks all of the array or none of it. If the stream has 
    been configured to swap bytes using stream_set_swap_bytes then this function will 
    swap the order of the bytes within each element that it peeks before storing them in the 
    given buffer.

    The stream_peek_begin function must be called to initialize the peek state. No data
    peeked will be removed from the input stream until stream_peek_end is called. If
    stream_peek_end is not called before the next stream_peek_begin then the peeked
    data is read again.

    See the stream_peek_begin function for more information and an example.

    The size of the stream receive buffer must be at least num_elements * sizeof(t_long)
    bytes in length.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is peeked. Blocking may occur because 
    data may be read from the underlying communication channel. However, the contents of 
    the stream buffer are never overwritten until stream_peek_end is called to indicate 
    that the peeked data may be removed from the input stream.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    the entire array then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to peek than the size of the entire array. Otherwise it returns 1. Once
    there are fewer bytes left to peek than the size of the entire array then it will
    return 0 to indicate the connection has been closed. Use stream_peek to peek any
    remaining bytes if required. If an error occurs, then it returns a negative error code.

    This function does not support two threads peeking data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    peeked.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    state        = the "peek state" initialized using stream_peek_begin
    buffer       = a pointer to an array of long integers in which the peeked data 
                   will be stored. It must be at least num_elements in length.
    num_elements = the number of elements to peek

    Return value:

    Returns 1 on success. If not enough data is available and the connection has been
    closed gracefully then 0 is returned. If an error occurs then a negative error code
    is returned.
*/
EXTERN t_int
stream_peek_long_array(t_stream stream, t_stream_peek_state * state, t_long * buffer, t_int num_elements);

/*
    Name:   stream_peek_single

    Description:

    This function peeks ahead in the input stream to read a single-precision (32-bit) 
    floating-point number from the stream. If the stream has been configured to swap 
    bytes using stream_set_swap_bytes then this function will swap the order of the bytes
    that it peeks before storing them in the given buffer.

    The stream_peek_begin function must be called to initialize the peek state. No data
    peeked will be removed from the input stream until stream_peek_end is called. If
    stream_peek_end is not called before the next stream_peek_begin then the peeked
    data is read again.

    See the stream_peek_begin function for more information and an example.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is peeked. Blocking may occur because 
    data may be read from the underlying communication channel. However, the contents of 
    the stream buffer are never overwritten until stream_peek_end is called to indicate 
    that the peeked data may be removed from the input stream.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of a
    single-precision floating-point number then it returns -QERR_WOULD_BLOCK. In this case, 
    the stream_poll function may be used with the STREAM_POLL_RECEIVE flag to determine when
    data becomes available. Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to peek than the size of a single-precision floating-point number. Otherwise 
    it returns the number of single-precision floating-point numbers peeked before the 
    connection closed. Once there are fewer bytes left to peek than the size of a 
    single-precision floating-point number then it will return 0 to indicate the connection 
    has been closed. Use stream_peek to peek any remaining bytes if required. If an error 
    occurs, then it returns a negative error code.

    This function does not support two threads peeking data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    peeked.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    state        = the "peek state" initialized using stream_peek_begin
    value        = a pointer to a variable in which the peeked data will be stored.

    Return value:

    Returns 1 if the data is successfully peeked from the stream. If fewer bytes than the size of 
    a single-precision floating-point number are available and the connection has been closed 
    gracefully then 0 is returned. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_peek_single(t_stream stream, t_stream_peek_state * state, t_single * value);

/*
    Name:   stream_peek_single_array

    Description:

    This function peeks ahead in the input stream to read an array of single-precision (32-bit) 
    floating-point numbers from a client stream. It either peeks all of the array or none of it. 
    If the stream has been configured to swap bytes using stream_set_swap_bytes then this
    function will swap the order of the bytes within each element that it peeks before storing them 
    in the given buffer.

    The stream_peek_begin function must be called to initialize the peek state. No data
    peeked will be removed from the input stream until stream_peek_end is called. If
    stream_peek_end is not called before the next stream_peek_begin then the peeked
    data is read again.

    See the stream_peek_begin function for more information and an example.

    The size of the stream receive buffer must be at least num_elements * sizeof(t_single)
    bytes in length.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is peeked. Blocking may occur because 
    data may be read from the underlying communication channel. However, the contents of 
    the stream buffer are never overwritten until stream_peek_end is called to indicate 
    that the peeked data may be removed from the input stream.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    the entire array then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to peek than the size of the entire array. Otherwise it returns 1. Once
    there are fewer bytes left to peek than the size of the entire array then it will
    return 0 to indicate the connection has been closed. Use stream_peek to peek any
    remaining bytes if required. If an error occurs, then it returns a negative error code.

    This function does not support two threads peeking data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    peeked.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    state        = the "peek state" initialized using stream_peek_begin
    buffer       = a pointer to an array of single-precision floating-point numbers in 
                   which the peeked data will be stored. It must be at least num_elements 
                   in length.
    num_elements = the number of elements to peek

    Return value:

    Returns 1 on success. If not enough data is available and the connection has been
    closed gracefully then 0 is returned. If an error occurs then a negative error code
    is returned.
*/
EXTERN t_int
stream_peek_single_array(t_stream stream, t_stream_peek_state * state, t_single * buffer, t_int num_elements);

/*
    Name:   stream_peek_double

    Description:

    This function peeks ahead in the input stream to read a double-precision (64-bit) 
    floating-point number from the stream. If the stream has been configured to swap 
    bytes using stream_set_swap_bytes then this function will swap the order of the bytes
    that it peeks before storing them in the given buffer.

    The stream_peek_begin function must be called to initialize the peek state. No data
    peeked will be removed from the input stream until stream_peek_end is called. If
    stream_peek_end is not called before the next stream_peek_begin then the peeked
    data is read again.

    See the stream_peek_begin function for more information and an example.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is peeked. Blocking may occur because 
    data may be read from the underlying communication channel. However, the contents of 
    the stream buffer are never overwritten until stream_peek_end is called to indicate 
    that the peeked data may be removed from the input stream.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of a
    double-precision floating-point number then it returns -QERR_WOULD_BLOCK. In this case, 
    the stream_poll function may be used with the STREAM_POLL_RECEIVE flag to determine when
    data becomes available. Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to peek than the size of a double-precision floating-point number. Otherwise 
    it returns the number of double-precision floating-point numbers peeked before the 
    connection closed. Once there are fewer bytes left to peek than the size of a 
    double-precision floating-point number then it will return 0 to indicate the connection 
    has been closed. Use stream_peek to peek any remaining bytes if required. If an error 
    occurs, then it returns a negative error code.

    This function does not support two threads peeking data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    peeked.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    state        = the "peek state" initialized using stream_peek_begin
    value        = a pointer to a variable in which the peeked data will be stored.

    Return value:

    Returns 1 if the data is successfully peeked from the stream. If fewer bytes than the size of 
    a double-precision floating-point number are available and the connection has been closed 
    gracefully then 0 is returned. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_peek_double(t_stream stream, t_stream_peek_state * state, t_double * value);

/*
    Name:   stream_peek_double_array

    Description:

    This function peeks ahead in the input stream to read an array of double-precision (64-bit) 
    floating-point numbers from a client stream. It either peeks all of the array or none of it. 
    If the stream has been configured to swap bytes using stream_set_swap_bytes then this
    function will swap the order of the bytes within each element that it peeks before storing them 
    in the given buffer.

    The stream_peek_begin function must be called to initialize the peek state. No data
    peeked will be removed from the input stream until stream_peek_end is called. If
    stream_peek_end is not called before the next stream_peek_begin then the peeked
    data is read again.

    See the stream_peek_begin function for more information and an example.

    The size of the stream receive buffer must be at least num_elements * sizeof(t_double)
    bytes in length.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is peeked. Blocking may occur because 
    data may be read from the underlying communication channel. However, the contents of 
    the stream buffer are never overwritten until stream_peek_end is called to indicate 
    that the peeked data may be removed from the input stream.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    the entire array then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to peek than the size of the entire array. Otherwise it returns 1. Once
    there are fewer bytes left to peek than the size of the entire array then it will
    return 0 to indicate the connection has been closed. Use stream_peek to peek any
    remaining bytes if required. If an error occurs, then it returns a negative error code.

    This function does not support two threads peeking data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    peeked.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    state        = the "peek state" initialized using stream_peek_begin
    buffer       = a pointer to an array of double-precision floating-point numbers in 
                   which the peeked data will be stored. It must be at least num_elements 
                   in length.
    num_elements = the number of elements to peek

    Return value:

    Returns 1 on success. If not enough data is available and the connection has been
    closed gracefully then 0 is returned. If an error occurs then a negative error code
    is returned.
*/
EXTERN t_int
stream_peek_double_array(t_stream stream, t_stream_peek_state * state, t_double * buffer, t_int num_elements);

/*
    Name:   stream_peek_utf8_char_array

    Description:

    This function peeks ahead in the input stream to read an array of UTF-8 characters from
    a client stream. It returns exactly the number of code units requested. Since UTF-8 
    characters are variable-sized and may be up to four code units (bytes) long, this function 
    may return an error if the characters received do not fit exactly within the number of 
    code units requested, since it only extracts complete UTF-8 characters from the input 
    stream. The return value is 1 if the entire array is peeked. It returns a negative error 
    code if an error occurs. If the peer closes the stream gracefully then zero is returned.

    If an illegal UTF-8 character is encountered then -QERR_ILLEGAL_UTF8_CHARACTER is
    returned.

    If an illegal UTF-16 character is encountered then -QERR_ILLEGAL_UTF16_CHARACTER is
    returned.

    If an illegal UTF-32 character is encountered then -QERR_ILLEGAL_UTF32_CHARACTER is
    returned.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then this
    function will swap the order of the bytes within each element that it peeks before 
    storing them in the given buffer.

    The stream_peek_begin function must be called to initialize the peek state. No data
    peeked will be removed from the input stream until stream_peek_end is called. If
    stream_peek_end is not called before the next stream_peek_begin then the peeked
    data is read again.

    See the stream_peek_begin function for more information and an example.

    The size of the stream receive buffer must be at least max_code_units * sizeof(t_utf8_char)
    bytes in length.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is peeked. Blocking may occur because 
    data may be read from the underlying communication channel. However, the contents of 
    the stream buffer are never overwritten until stream_peek_end is called to indicate 
    that the peeked data may be removed from the input stream.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer code units are available then the size of
    the entire array then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    code units left to peek than the size of the entire array. Otherwise it returns 1. Once
    there are fewer code units left to peek than the size of the entire array then it will
    return 0 to indicate the connection has been closed. Use stream_peek to peek any
    remaining bytes if required. If an error occurs, then it returns a negative error code.

    This function does not support two threads peeking data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    peeked.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    state          = the "peek state" initialized using stream_peek_begin
    buffer         = a pointer to an array of t_utf8_char's in which the received characters
                     will be stored.
    max_code_units = the size of the buffer in code units (bytes)

    Return value:

    Returns 1 upon success. If fewer code units than the size of the array are available and the 
    connection has been closed gracefully then 0 is returned. If an error occurs then a 
    negative error code is returned.
*/
EXTERN t_int
stream_peek_utf8_char_array(t_stream stream, t_stream_peek_state * state, t_utf8_char * buffer, t_int max_code_units);

/*
    Name:   stream_peek_utf16_char_array

    Description:

    This function peeks ahead in the input stream to read an array of UTF-16 characters from
    a client stream. It returns exactly the number of code units requested. Since UTF-16 
    characters are variable-sized and may be up to two code units (16-bit words) long, this function 
    may return an error if the characters received do not fit exactly within the number of 
    code units requested, since it only extracts complete UTF-16 characters from the input 
    stream. The return value is 1 if the entire array is peeked. It returns a negative error 
    code if an error occurs. If the peer closes the stream gracefully then zero is returned.

    If an illegal UTF-8 character is encountered then -QERR_ILLEGAL_UTF8_CHARACTER is
    returned.

    If an illegal UTF-16 character is encountered then -QERR_ILLEGAL_UTF16_CHARACTER is
    returned.

    If an illegal UTF-32 character is encountered then -QERR_ILLEGAL_UTF32_CHARACTER is
    returned.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then this
    function will swap the order of the bytes within each element that it peeks before 
    storing them in the given buffer.

    The stream_peek_begin function must be called to initialize the peek state. No data
    peeked will be removed from the input stream until stream_peek_end is called. If
    stream_peek_end is not called before the next stream_peek_begin then the peeked
    data is read again.

    See the stream_peek_begin function for more information and an example.

    The size of the stream receive buffer must be at least max_code_units * sizeof(t_utf16_char)
    bytes in length.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is peeked. Blocking may occur because 
    data may be read from the underlying communication channel. However, the contents of 
    the stream buffer are never overwritten until stream_peek_end is called to indicate 
    that the peeked data may be removed from the input stream.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer code units are available then the size of
    the entire array then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    code units left to peek than the size of the entire array. Otherwise it returns 1. Once
    there are fewer code units left to peek than the size of the entire array then it will
    return 0 to indicate the connection has been closed. Use stream_peek to peek any
    remaining bytes if required. If an error occurs, then it returns a negative error code.

    This function does not support two threads peeking data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    peeked.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    state          = the "peek state" initialized using stream_peek_begin
    buffer         = a pointer to an array of t_utf16_char's in which the received characters
                     will be stored.
    max_code_units = the size of the buffer in code units (16-bit words)

    Return value:

    Returns 1 upon success. If fewer code units than the size of the array are available and the 
    connection has been closed gracefully then 0 is returned. If an error occurs then a 
    negative error code is returned.
*/
EXTERN t_int
stream_peek_utf16_char_array(t_stream stream, t_stream_peek_state * state, t_utf16_char * buffer, t_int max_code_units);

/*
    Name:   stream_peek_utf32_char_array

    Description:

    This function peeks ahead in the input stream to read an array of UTF-32 characters from
    a client stream. It returns exactly the number of code units requested. The return value 
    is 1 if the entire array is peeked. It returns a negative error code if an error occurs. 
    If the peer closes the stream gracefully then zero is returned.

    If an illegal UTF-8 character is encountered then -QERR_ILLEGAL_UTF8_CHARACTER is
    returned.

    If an illegal UTF-16 character is encountered then -QERR_ILLEGAL_UTF16_CHARACTER is
    returned.

    If an illegal UTF-32 character is encountered then -QERR_ILLEGAL_UTF32_CHARACTER is
    returned.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then this
    function will swap the order of the bytes within each element that it peeks before 
    storing them in the given buffer.

    The stream_peek_begin function must be called to initialize the peek state. No data
    peeked will be removed from the input stream until stream_peek_end is called. If
    stream_peek_end is not called before the next stream_peek_begin then the peeked
    data is read again.

    See the stream_peek_begin function for more information and an example.

    The size of the stream receive buffer must be at least max_code_units * sizeof(t_utf32_char)
    bytes in length.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is peeked. Blocking may occur because 
    data may be read from the underlying communication channel. However, the contents of 
    the stream buffer are never overwritten until stream_peek_end is called to indicate 
    that the peeked data may be removed from the input stream.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer code units are available then the size of
    the entire array then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    code units left to peek than the size of the entire array. Otherwise it returns 1. Once
    there are fewer code units left to peek than the size of the entire array then it will
    return 0 to indicate the connection has been closed. Use stream_peek to peek any
    remaining bytes if required. If an error occurs, then it returns a negative error code.

    This function does not support two threads peeking data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    peeked.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    state          = the "peek state" initialized using stream_peek_begin
    buffer         = a pointer to an array of t_utf32_char's in which the received characters
                     will be stored.
    max_code_units = the size of the buffer in code units (32-bit words)

    Return value:

    Returns 1 upon success. If fewer code units than the size of the array are available and the 
    connection has been closed gracefully then 0 is returned. If an error occurs then a 
    negative error code is returned.
*/
EXTERN t_int
stream_peek_utf32_char_array(t_stream stream, t_stream_peek_state * state, t_utf32_char * buffer, t_int max_code_units);

/*
    Name:   stream_peek_unit

    Description:

    This function peeks ahead in the input stream to read a "unit" from the stream, where 
    the size of a "unit" is determined by the unit_size parameter. If the stream has been 
    configured to swap bytes using stream_set_swap_bytes then this function will swap the 
    order of the bytes that it peeks before storing them in the given buffer.

    The stream_peek_begin function must be called to initialize the peek state. No data
    peeked will be removed from the input stream until stream_peek_end is called. If
    stream_peek_end is not called before the next stream_peek_begin then the peeked
    data is read again.

    See the stream_peek_begin function for more information and an example.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is peeked. Blocking may occur because 
    data may be read from the underlying communication channel. However, the contents of 
    the stream buffer are never overwritten until stream_peek_end is called to indicate 
    that the peeked data may be removed from the input stream.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of a
    unit then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll function may be
    used with the STREAM_POLL_RECEIVE flag to determine when data becomes available.
    Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to peek than the size of a unit. Otherwise it returns 1. Once there are fewer 
    bytes left to peek than the size of a unit then it will return 0 to indicate the connection
    has been closed. Use stream_peek to peek any remaining bytes if required. If an error 
    occurs, then it returns a negative error code.

    This function does not support two threads peeking data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    peeked.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream      = a client stream established using stream_connect or stream_accept.
    state       = the "peek state" initialized using stream_peek_begin
    unit        = a pointer to a variable in which the peeked data will be stored. It
                  must be unit_size bytes in size.
    unit_size   = the size of a unit

    Return value:

    Returns 1 if the data is successfully peeked from the stream. If fewer bytes than the size of 
    a unit are available and the connection has been closed gracefully then 0 is returned. If an 
    error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_peek_unit(t_stream stream, t_stream_peek_state * state, void * unit, t_int unit_size);

/*
    Name:   stream_peek_unit_array

    Description:

    This function peeks ahead in the input stream to read an array of "units" from a client 
    stream, where the size of a "unit" is determined by the unit_size parameter. It either 
    peeks all of the array or none of it. If the stream has been configured to swap bytes 
    using stream_set_swap_bytes then this function will swap the order of the bytes 
    within each unit that it peeks before storing them in the given buffer.

    The stream_peek_begin function must be called to initialize the peek state. No data
    peeked will be removed from the input stream until stream_peek_end is called. If
    stream_peek_end is not called before the next stream_peek_begin then the peeked
    data is read again.

    See the stream_peek_begin function for more information and an example.

    The size of the stream receive buffer must be at least num_units * unit_size bytes in 
    length.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function blocks until all the data is peeked. Blocking may occur because 
    data may be read from the underlying communication channel. However, the contents of 
    the stream buffer are never overwritten until stream_peek_end is called to indicate 
    that the peeked data may be removed from the input stream.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. If fewer bytes are available then the size of
    the entire array then it returns -QERR_WOULD_BLOCK. In this case, the stream_poll 
    function may be used with the STREAM_POLL_RECEIVE flag to determine when data becomes
    available. Otherwise it returns 1.

    If the connection has been closed gracefully then it returns 0 only if there are fewer
    bytes left to peek than the size of the entire array. Otherwise it returns 1. Once
    there are fewer bytes left to peek than the size of the entire array then it will
    return 0 to indicate the connection has been closed. Use stream_peek to peek any
    remaining bytes if required. If an error occurs, then it returns a negative error code.

    This function does not support two threads peeking data at the same time. However,
    data may be sent or the stream flushed by another thread at the same time as data is being
    peeked.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    state        = the "peek state" initialized using stream_peek_begin
    units        = a pointer to an array of units in which the peeked data will be stored. 
                   It must be at least num_units in length.
    unit_size    = the size of a unit in bytes
    num_units    = the number of units to peek

    Return value:

    Returns 1 on success. If not enough data is available and the connection has been
    closed gracefully then 0 is returned. If an error occurs then a negative error code
    is returned.
*/
EXTERN t_int
stream_peek_unit_array(t_stream stream, t_stream_peek_state * state, void * units, t_int unit_size, t_int num_units);

/*
    Name:   stream_peek_end

    Description:

    This function finishes a "peek" operation by removing all peeked data from the input
    stream. It must be called after a sequence of stream_peek_xxxx operations to complete
    the peek. If the status argument is one then it advances the input stream and the data
    is removed from the input stream to make room for new data. If the status argument is
    less than one then the input stream is not advanced. In that case, the data will remain
    in the input stream and will be read the next time the user receives data from the stream.

    The status argument is generally the result of the last stream_peek_xxxx operation.
    Refer to the stream_peek_begin function for an example.

    This function assumes that the stream is valid. It does not block because it does not
    access the underlying communication channel.

    Parameters:

    stream    = a client stream established using stream_connect or stream_accept.
    state     = the "peek state" indicating how much data to remove from the input stream.
    status    = this value is typically the result from the last stream_peek_xxxx operation.
                If it is 1 then the peek is completed and the stream pointer advanced. If it
                is 0 beceause the stream closed before finishing the peek operations then this
                function does not advance the stream pointer and returns 0. If it is negative
                then the stream pointer is not advanced and the status is returned as the result.

    Return value:

    This function returns an error if one of the parameters is invalid or the
    stream is shutdown or closed, or the status argument is negative. It returns
    one if the status argument is one and the peek is completed successfully. It
    returns zero if the status argument is zero.
*/
EXTERN t_error
stream_peek_end(t_stream stream, t_stream_peek_state * state, t_error status);

/*
    Name:   stream_poke_begin

    Description:

    This function prepares the t_stream_poke_state to begin poking data to the stream. "Poking" involves
    writing to the stream buffer without writing to the underlying communication channel or updating
    the buffer pointers. It is particularly useful for writing "atomic" data to the stream whose
    length is not known in advance, such as characters being converted to the underlying character
    format of the stream. The "poke state" is used to keep track of how much data has been poked
    into the stream so far.
 
    The stream_poke_end function must be called to end the poke operation. Failure to do so
    may potentially cause deadlock the next time stream_poke_begin is called.
 
    As an example of using the poke capabilities to write a sequence of data types as a single
    atomic unit using non-blocking I/O:

        t_stream_poke_state state;

        result = stream_poke_begin(stream, &state);
        if (result == 0) {
            result = stream_poke_int(stream, &state, 5);
            if (result > 0) {
                result = stream_poke_double(stream, &state, 3.14);
            }
 
            result = stream_poke_end(stream, &state, result);
            if (result > 0)
                flush_result = stream_flush(stream);
        }
 
    In this example, the code pokes an integer and a double into the output stream. The data
    is not transmitted over the underlying communication channel but is maintained in the
    stream buffer. If at any time the operation would block because there is not enough space
    in the stream buffer, then -QERR_WOULD_BLOCK is returned. In this case, stream_poke_end
    does not advance the output stream and the poked data is discarded and not sent to the 
    output stream. Hence, the next time the code is called it may write the same data again 
    without the peer receiving two copies. The stream_poke_end returns the same error,
    -QERR_WOULD_BLOCK, to make it easier to propagate the error code.

    However, if the stream_poke_int and stream_poke_double succeed then when stream_poke_end is
    called the stream pointer is advanced. Hence, the data will be sent to the output
    stream the next time the stream is flushed and subsequent sends/pokes to the stream 
    will send new data. In this case, stream_poke_end returns one.

    Thus, the integer and double are sent as one atomic unit to the output stream, because
    if not enough space is available in the stream buffer, then nothing is written to the
    output stream. Only if both quantities are written successfully is the data actually 
    sent to the output stream.

    Note that the stream_poke_xxxx functions may send data to the underlying communication
    channel (that remained from previous send operations), but the data being poked is never 
    sent until stream_poke_end is called to indicate that the poked data may be sent to the 
    output stream.

    This function assumes that the stream is valid. It does not block because it does not
    access the underlying communication channel.

    Parameters:

    stream    = a client stream established using stream_connect or stream_accept.
    state     = the "poke state" to initialize.

    Return value:

    This function only returns an error if one of the parameters is invalid or the
    stream is shutdown or closed.
*/
EXTERN t_error
stream_poke_begin(t_stream stream, t_stream_poke_state * state);

/*
    Name:   stream_poke_byte

    Description:

    This function writes a byte to the stream buffer. It attempts to store the byte in 
    the stream buffer. If there is enough room available in the stream buffer then it stores the
    data in the buffer and returns immediately. However, the stream pointer is not
    advanced, so the data is not written to the actual communication channel. The data
    will only be written to the underlying communication channel if stream_poke_end is
    called. If an error occurs, then it returns a negative error code. If the connection 
    is closed it is considered an error condition.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer (to write out data
    previously sent and still waiting in the stream buffer). Once the byte is poked into the
    buffer then 1 is returned. If an error occurs then the error code is returned and the 
    stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 if the data is poked successfully. 
    If the byte could not be poked without blocking, then -QERR_WOULD_BLOCK is returned. 
    If an error occurs then the error code is returned and the stream should be closed.

    This function does not support two threads poking or flushing data at the same time.
    However, data may be poked or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream    = a client stream established using stream_connect or stream_accept.
    state     = the poke state initialized using stream_poke_begin.
    byte      = the value to be poked.

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_poke_byte(t_stream stream, t_stream_poke_state * state, t_byte byte);

/*
    Name:   stream_poke_byte_array

    Description:

    This function writes an array of bytes to the stream buffer. It attempts to store the
    bytes in the stream buffer. It either writes all of the array or none of it. 
    If there is enough room available in the stream buffer then it stores the data 
    in the buffer and returns immediately. However, the stream pointer is not
    advanced, so the data is not written to the actual communication channel. The data
    will only allowed to be written to the underlying communication channel if 
    stream_poke_end is called. If an error occurs, then it returns a negative error 
    code. If the connection is closed it is considered an error condition.

    The size of the stream poke buffer must be at least as large as num_elements
    bytes.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer (to write out data
    previously sent and still waiting in the stream buffer). Once the entire array is poked 
    into the buffer then 1 is returned. If an error occurs then the error code is returned 
    and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 on success. If the entire array could not be 
    poked without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs then the 
    error code is returned and the stream should be closed.

    This function does not support two threads poking or flushing data at the same time.
    However, data may be poked or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    state        = the poke state initialized using stream_poke_begin.
    bytes        = the array of bytes to be poked.
    num_elements = the number of elements in the array to poke

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_poke_byte_array(t_stream stream, t_stream_poke_state * state, const t_byte * bytes, t_uint num_elements);

/*
    Name:   stream_poke_short

    Description:

    This function writes a short (16-bit) integer to the stream buffer. It attempts to 
    store the short integer in the stream buffer. If there is enough room available in the 
    stream buffer then it stores the data in the buffer and returns immediately. However, 
    the stream pointer is not advanced, so the data is not written to the actual communication 
    channel. The data will only be written to the underlying communication channel if 
    stream_poke_end is called. If an error occurs, then it returns a negative error code. 
    If the connection is closed it is considered an error condition.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the short integer when they are poked.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer (to write out data
    previously sent and still waiting in the stream buffer). Once the short integer is poked 
    into the buffer then 1 is returned. If an error occurs then the error code is returned 
    and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 if the data is poked successfully. 
    If the short integer could not be poked without blocking, then -QERR_WOULD_BLOCK is 
    returned. If an error occurs then the error code is returned and the stream should be 
    closed.

    This function does not support two threads poking or flushing data at the same time.
    However, data may be poked or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream     = a client stream established using stream_connect or stream_accept.
    state      = the poke state initialized using stream_poke_begin.
    value      = the value to be poked.

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_poke_short(t_stream stream, t_stream_poke_state * state, t_short value);

/*
    Name:   stream_poke_short_array

    Description:

    This function writes an array of short (16-bit) integers to the stream buffer. 
    It attempts to store the short integers in the stream buffer. It either writes
    all of the array or none of it. If there is enough room available in the stream 
    buffer then it stores the data in the buffer and returns immediately. However, 
    the stream pointer is not advanced, so the data is not written to the actual 
    communication channel. The data will only allowed to be written to the underlying
    communication channel if stream_poke_end is called. If an error occurs, then it 
    returns a negative error code. If the connection is closed it is considered an 
    error condition.

    The size of the stream poke buffer must be at least as large as 
    num_elements * sizeof(t_short) bytes.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each short integer when they 
    are poked.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer (to write out data
    previously sent and still waiting in the stream buffer). Once the entire array is poked 
    into the buffer then 1 is returned. If an error occurs then the error code is returned 
    and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 on success. If the entire array could not be 
    poked without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs then the 
    error code is returned and the stream should be closed.

    This function does not support two threads poking or flushing data at the same time.
    However, data may be poked or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    state        = the poke state initialized using stream_poke_begin.
    buffer       = the array of short integers to be poked.
    num_elements = the number of elements in the array to poke

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_poke_short_array(t_stream stream, t_stream_poke_state * state, const t_short * buffer, t_uint num_elements);

/*
    Name:   stream_poke_int

    Description:

    This function writes a 32-bit integer to the stream buffer. It attempts to 
    store the integer in the stream buffer. If there is enough room available in the 
    stream buffer then it stores the data in the buffer and returns immediately. However, 
    the stream pointer is not advanced, so the data is not written to the actual communication 
    channel. The data will only be written to the underlying communication channel if 
    stream_poke_end is called. If an error occurs, then it returns a negative error code. 
    If the connection is closed it is considered an error condition.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the integer when they are poked.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer (to write out data
    previously sent and still waiting in the stream buffer). Once the integer is poked 
    into the buffer then 1 is returned. If an error occurs then the error code is returned 
    and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 if the data is poked successfully. 
    If the integer could not be poked without blocking, then -QERR_WOULD_BLOCK is 
    returned. If an error occurs then the error code is returned and the stream should be 
    closed.

    This function does not support two threads poking or flushing data at the same time.
    However, data may be poked or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream   = a client stream established using stream_connect or stream_accept.
    state    = the poke state initialized using stream_poke_begin.
    value    = the value to be poked.

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_poke_int(t_stream stream, t_stream_poke_state * state, t_int value);

/*
    Name:   stream_poke_int_array

    Description:

    This function writes an array of 32-bit integers to the stream buffer. 
    It attempts to store the integers in the stream buffer. It either writes
    all of the array or none of it. If there is enough room available in the stream 
    buffer then it stores the data in the buffer and returns immediately. However, 
    the stream pointer is not advanced, so the data is not written to the actual 
    communication channel. The data will only allowed to be written to the underlying
    communication channel if stream_poke_end is called. If an error occurs, then it 
    returns a negative error code. If the connection is closed it is considered an 
    error condition.

    The size of the stream poke buffer must be at least as large as 
    num_elements * sizeof(t_int) bytes.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each integer when they 
    are poked.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer (to write out data
    previously sent and still waiting in the stream buffer). Once the entire array is poked 
    into the buffer then 1 is returned. If an error occurs then the error code is returned 
    and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 on success. If the entire array could not be 
    poked without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs then the 
    error code is returned and the stream should be closed.

    This function does not support two threads poking or flushing data at the same time.
    However, data may be poked or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    state        = the poke state initialized using stream_poke_begin.
    buffer       = the array of integers to be poked.
    num_elements = the number of elements in the array to poke

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_poke_int_array(t_stream stream, t_stream_poke_state * state, const t_int * buffer, t_uint num_elements);

/*
    Name:   stream_poke_long

    Description:

    This function writes a long (64-bit) integer to the stream buffer. It attempts to 
    store the long integer in the stream buffer. If there is enough room available in the 
    stream buffer then it stores the data in the buffer and returns immediately. However, 
    the stream pointer is not advanced, so the data is not written to the actual communication 
    channel. The data will only be written to the underlying communication channel if 
    stream_poke_end is called. If an error occurs, then it returns a negative error code. 
    If the connection is closed it is considered an error condition.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the long integer when they are poked.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer (to write out data
    previously sent and still waiting in the stream buffer). Once the long integer is poked 
    into the buffer then 1 is returned. If an error occurs then the error code is returned 
    and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 if the data is poked successfully. 
    If the long integer could not be poked without blocking, then -QERR_WOULD_BLOCK is 
    returned. If an error occurs then the error code is returned and the stream should be 
    closed.

    This function does not support two threads poking or flushing data at the same time.
    However, data may be poked or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream     = a client stream established using stream_connect or stream_accept.
    state      = the poke state initialized using stream_poke_begin.
    value      = the value to be poked.

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_poke_long(t_stream stream, t_stream_poke_state * state, t_long value);

/*
    Name:   stream_poke_long_array

    Description:

    This function writes an array of long (64-bit) integers to the stream buffer. 
    It attempts to store the long integers in the stream buffer. It either writes
    all of the array or none of it. If there is enough room available in the stream 
    buffer then it stores the data in the buffer and returns immediately. However, 
    the stream pointer is not advanced, so the data is not written to the actual 
    communication channel. The data will only allowed to be written to the underlying
    communication channel if stream_poke_end is called. If an error occurs, then it 
    returns a negative error code. If the connection is closed it is considered an 
    error condition.

    The size of the stream poke buffer must be at least as large as 
    num_elements * sizeof(t_long) bytes.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each long integer when they 
    are poked.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer (to write out data
    previously sent and still waiting in the stream buffer). Once the entire array is poked 
    into the buffer then 1 is returned. If an error occurs then the error code is returned 
    and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 on success. If the entire array could not be 
    poked without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs then the 
    error code is returned and the stream should be closed.

    This function does not support two threads poking or flushing data at the same time.
    However, data may be poked or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    state        = the poke state initialized using stream_poke_begin.
    buffer       = the array of long integers to be poked.
    num_elements = the number of elements in the array to poke

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_poke_long_array(t_stream stream, t_stream_poke_state * state, const t_long * buffer, t_uint num_elements);

/*
    Name:   stream_poke_single

    Description:

    This function writes a single-precision (32-bit) floating-point number to the stream buffer. 
    It attempts to store the floating-point number in the stream buffer. If there is enough room 
    available in the stream buffer then it stores the data in the buffer and returns immediately. 
    However, the stream pointer is not advanced, so the data is not written to the actual communication 
    channel. The data will only be written to the underlying communication channel if 
    stream_poke_end is called. If an error occurs, then it returns a negative error code. 
    If the connection is closed it is considered an error condition.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the single floating-point number 
    when they are poked.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer (to write out data
    previously sent and still waiting in the stream buffer). Once the single-precision floating-point 
    number is poked into the buffer then 1 is returned. If an error occurs then the error code is returned 
    and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 if the data is poked successfully. 
    If the single-precision floating-point number could not be poked without blocking, then 
    -QERR_WOULD_BLOCK is returned. If an error occurs then the error code is returned and the 
    stream should be closed.

    This function does not support two threads poking or flushing data at the same time.
    However, data may be poked or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream     = a client stream established using stream_connect or stream_accept.
    state      = the poke state initialized using stream_poke_begin.
    value      = the value to be poked.

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_poke_single(t_stream stream, t_stream_poke_state * state, t_single value);

/*
    Name:   stream_poke_single_array

    Description:

    This function writes an array of single-precision (32-bit) floating-point numbers 
    to the stream buffer. It attempts to store the single floating-point numbers in 
    the stream buffer. It either writes all of the array or none of it. If there is 
    enough room available in the stream buffer then it stores the data in the buffer 
    and returns immediately. However, the stream pointer is not advanced, so the data 
    is not written to the actual communication channel. The data will only allowed to 
    be written to the underlying communication channel if stream_poke_end is called. 
    If an error occurs, then it returns a negative error code. If the connection is 
    closed it is considered an error condition.

    The size of the stream poke buffer must be at least as large as 
    num_elements * sizeof(t_single) bytes.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each single-precision 
    floating-point number when they are poked.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer (to write out data
    previously sent and still waiting in the stream buffer). Once the entire array is poked 
    into the buffer then 1 is returned. If an error occurs then the error code is returned 
    and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 on success. If the entire array could not be 
    poked without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs then the 
    error code is returned and the stream should be closed.

    This function does not support two threads poking or flushing data at the same time.
    However, data may be poked or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    state        = the poke state initialized using stream_poke_begin.
    buffer       = the array of single-precision floating-point numbers to be poked.
    num_elements = the number of elements in the array to poke

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_poke_single_array(t_stream stream, t_stream_poke_state * state, const t_single * buffer, t_uint num_elements);

/*
    Name:   stream_poke_double

    Description:

    This function writes a double-precision (64-bit) floating-point number to the stream buffer. 
    It attempts to store the floating-point number in the stream buffer. If there is enough room 
    available in the stream buffer then it stores the data in the buffer and returns immediately. 
    However, the stream pointer is not advanced, so the data is not written to the actual communication 
    channel. The data will only be written to the underlying communication channel if 
    stream_poke_end is called. If an error occurs, then it returns a negative error code. 
    If the connection is closed it is considered an error condition.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the double floating-point number 
    when they are poked.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer (to write out data
    previously sent and still waiting in the stream buffer). Once the double-precision floating-point 
    number is poked into the buffer then 1 is returned. If an error occurs then the error code is returned 
    and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 if the data is poked successfully. 
    If the double-precision floating-point number could not be poked without blocking, then 
    -QERR_WOULD_BLOCK is returned. If an error occurs then the error code is returned and the 
    stream should be closed.

    This function does not support two threads poking or flushing data at the same time.
    However, data may be poked or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream     = a client stream established using stream_connect or stream_accept.
    state      = the poke state initialized using stream_poke_begin.
    value      = the value to be poked.

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_poke_double(t_stream stream, t_stream_poke_state * state, t_double value);

/*
    Name:   stream_poke_double_array

    Description:

    This function writes an array of double-precision (64-bit) floating-point numbers 
    to the stream buffer. It attempts to store the double floating-point numbers in 
    the stream buffer. It either writes all of the array or none of it. If there is 
    enough room available in the stream buffer then it stores the data in the buffer 
    and returns immediately. However, the stream pointer is not advanced, so the data 
    is not written to the actual communication channel. The data will only allowed to 
    be written to the underlying communication channel if stream_poke_end is called. 
    If an error occurs, then it returns a negative error code. If the connection is 
    closed it is considered an error condition.

    The size of the stream poke buffer must be at least as large as 
    num_elements * sizeof(t_double) bytes.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each double-precision 
    floating-point number when they are poked.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer (to write out data
    previously sent and still waiting in the stream buffer). Once the entire array is poked 
    into the buffer then 1 is returned. If an error occurs then the error code is returned 
    and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 on success. If the entire array could not be 
    poked without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs then the 
    error code is returned and the stream should be closed.

    This function does not support two threads poking or flushing data at the same time.
    However, data may be poked or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    state        = the poke state initialized using stream_poke_begin.
    buffer       = the array of double-precision floating-point numbers to be poked.
    num_elements = the number of elements in the array to poke

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_poke_double_array(t_stream stream, t_stream_poke_state * state, const t_double * buffer, t_uint num_elements);

/*
    Name:   stream_poke_utf8_char_array

    Description:

    This function writes an array of UTF-8 characters to the stream buffer. The characters 
    are converted according to the stream's character format prior to writing the characters
    to the stream buffer. It attempts to store the converted characters in the stream buffer. 
    It either writes all of the array or none of it. If there is enough room available in the
    stream buffer then it stores the data in the buffer and returns immediately. However, the 
    stream pointer is not advanced, so the data is not written to the actual communication 
    channel. The data will only allowed to be written to the underlying communication channel 
    if stream_poke_end is called. If an error occurs, then it returns a negative error code. 
    If the connection is closed it is considered an error condition.

    The size of the stream send buffer must be at least as large as the number of code units 
    being sent.

    Note that this function only writes complete characters to the stream. It will not write
    some but not all of the code units comprising a multi-unit character.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the converted character if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer (to write out data
    previously sent and still waiting in the stream buffer). Once the entire array is poked 
    into the buffer then 1 is returned. If an error occurs then the error code is returned 
    and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 on success. If the entire array could not be 
    poked without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs then the 
    error code is returned and the stream should be closed.

    This function does not support two threads poking or flushing data at the same time.
    However, data may be poked or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    state          = the poke state initialized using stream_poke_begin.
    buffer         = a pointer to an array of t_utf8_char's containing the characters
                     to send. A UTF-8 character may take up to 4 code units or bytes.
    code_units     = the size of the buffer in code units (bytes)

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_poke_utf8_char_array(t_stream stream, t_stream_poke_state * state, const t_utf8_char * buffer, t_int code_units);

/*
    Name:   stream_poke_utf16_char_array

    Description:

    This function writes an array of UTF-16 characters to the stream buffer. The characters 
    are converted according to the stream's character format prior to writing the characters
    to the stream buffer. It attempts to store the converted characters in the stream buffer. 
    It either writes all of the array or none of it. If there is enough room available in the
    stream buffer then it stores the data in the buffer and returns immediately. However, the 
    stream pointer is not advanced, so the data is not written to the actual communication 
    channel. The data will only allowed to be written to the underlying communication channel 
    if stream_poke_end is called. If an error occurs, then it returns a negative error code. 
    If the connection is closed it is considered an error condition.

    The size of the stream send buffer must be at least as large as the number of code units 
    being sent.

    Note that this function only writes complete characters to the stream. It will not write
    some but not all of the code units comprising a multi-unit character.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the converted character if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer (to write out data
    previously sent and still waiting in the stream buffer). Once the entire array is poked 
    into the buffer then 1 is returned. If an error occurs then the error code is returned 
    and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 on success. If the entire array could not be 
    poked without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs then the 
    error code is returned and the stream should be closed.

    This function does not support two threads poking or flushing data at the same time.
    However, data may be poked or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    state          = the poke state initialized using stream_poke_begin.
    buffer         = a pointer to an array of t_utf16_char's containing the characters
                     to send. A UTF-16 character may take up to 2 code units or words.
    code_units     = the size of the buffer in code units (words)

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_poke_utf16_char_array(t_stream stream, t_stream_poke_state * state, const t_utf16_char * buffer, t_int code_units);

/*
    Name:   stream_poke_utf32_char_array

    Description:

    This function writes an array of UTF-32 characters to the stream buffer. The characters 
    are converted according to the stream's character format prior to writing the characters
    to the stream buffer. It attempts to store the converted characters in the stream buffer. 
    It either writes all of the array or none of it. If there is enough room available in the
    stream buffer then it stores the data in the buffer and returns immediately. However, the 
    stream pointer is not advanced, so the data is not written to the actual communication 
    channel. The data will only allowed to be written to the underlying communication channel 
    if stream_poke_end is called. If an error occurs, then it returns a negative error code. 
    If the connection is closed it is considered an error condition.

    The size of the stream send buffer must be at least as large as the number of code units 
    being sent.

    Note that this function only writes complete characters to the stream. It will not write
    some but not all of the code units comprising a multi-unit character.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the converted character if the
    stream's character format is UTF-16 or UTF-32.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer (to write out data
    previously sent and still waiting in the stream buffer). Once the entire array is poked 
    into the buffer then 1 is returned. If an error occurs then the error code is returned 
    and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 on success. If the entire array could not be 
    poked without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs then the 
    error code is returned and the stream should be closed.

    This function does not support two threads poking or flushing data at the same time.
    However, data may be poked or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream         = a client stream established using stream_connect or stream_accept.
    state          = the poke state initialized using stream_poke_begin.
    buffer         = a pointer to an array of t_utf32_char's containing the characters
                     to send. A UTF-32 character is always one code unit or 32-bit word.
    code_units = the size of the buffer in code units (32-bit words)

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_poke_utf32_char_array(t_stream stream, t_stream_poke_state * state, const t_utf32_char * buffer, t_int code_units);

/*
    Name:   stream_poke_unit

    Description:

    This function writes a "unit" to the stream buffer, where the size of a "unit" is
    determined by the unit_size parameter. It attempts to store the unit in the stream
    buffer. If there is enough room available in the stream buffer then it stores the
    data in the buffer and returns immediately. However, the stream pointer is not
    advanced, so the data is not written to the actual communication channel. The data
    will only be written to the underlying communication channel if stream_poke_end is
    called. If an error occurs, then it returns a negative error code. If the connection 
    is closed it is considered an error condition.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within the unit when they are poked.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer (to write out data
    previously sent and still waiting in the stream buffer). Once the unit is poked into the
    buffer then 1 is returned. If an error occurs then the error code is returned and the 
    stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 if the data is poked successfully. 
    If the unit could not be poked without blocking, then -QERR_WOULD_BLOCK is returned. 
    If an error occurs then the error code is returned and the stream should be closed.

    This function does not support two threads poking or flushing data at the same time.
    However, data may be poked or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream    = a client stream established using stream_connect or stream_accept.
    state     = the poke state initialized using stream_poke_begin.
    unit      = a pointer to the value to be poked.
    unit_size = the size of the value

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_poke_unit(t_stream stream, t_stream_poke_state * state, const void * unit, t_int unit_size);

/*
    Name:   stream_poke_unit_array

    Description:

    This function writes an array of "units" to the stream buffer, where the size
    of a "unit" is determined by the unit_size parameter. It attempts to store the
    "units" in the stream buffer. It either writes all of the array or none of it. 
    If there is enough room available in the stream buffer then it stores the data 
    in the buffer and returns immediately. However, the stream pointer is not
    advanced, so the data is not written to the actual communication channel. The data
    will only allowed to be written to the underlying communication channel if 
    stream_poke_end is called. If an error occurs, then it returns a negative error 
    code. If the connection is closed it is considered an error condition.

    The size of the stream poke buffer must be at least as large as num_units * unit_size 
    bytes.

    If the stream has been configured to swap bytes using stream_set_swap_bytes then
    this function will swap the order of the bytes within each unit when they are poked.

    If stream_listen or stream_connect was called with the non-blocking flag set to false (0),
    then this function may block attempting to flush the stream buffer (to write out data
    previously sent and still waiting in the stream buffer). Once the entire array is poked 
    into the buffer then 1 is returned. If an error occurs then the error code is returned 
    and the stream should be closed.

    If stream_listen or stream_connect was called with the non-blocking flag set to true (1),
    then this function does not block. It returns 1 on success. If the entire array could not be 
    poked without blocking, then -QERR_WOULD_BLOCK is returned. If an error occurs then the 
    error code is returned and the stream should be closed.

    This function does not support two threads poking or flushing data at the same time.
    However, data may be poked or the stream flushed by another thread at the same time as
    data is being received.

    The BSD socket API has no equivalent to this function.

    Parameters:

    stream       = a client stream established using stream_connect or stream_accept.
    state        = the poke state initialized using stream_poke_begin.
    units        = the array to be poked.
    unit_size    = the size of one element in the array
    num_units    = the number of elements in the array to poke

    Return value:

    Returns 1 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_int
stream_poke_unit_array(t_stream stream, t_stream_poke_state * state, const void * units, t_int unit_size, t_int num_units);

/*
    Name:   stream_poke_end

    Description:

    This function finishes a "poke" operation by updating the stream buffer pointers so
    that all poked data is now marked as written to the stream. It must be called to end
    a poke operation.
 
    If the status argument is one then it advances the output stream and the data
    is sent to the underlying output stream to make room for new data. If the status argument
    is less than one then the poked data is discarded.
 
    The status argument is generally the result of the last stream_poke_xxxx operation.
    Refer to the stream_poke_begin function for an example.
 
    This function assumes that the stream is valid. It does not block because it does not
    access the underlying communication channel.
 
    Parameters:
 
    stream    = a client stream established using stream_connect or stream_accept.
    state     = the "poke state" indicating how much data to add to the output stream.
    status    = this value is typically the result from the last stream_poke_xxxx operation.
                If it is 1 then the poke is completed and the data is sent. If it is negative
                then the data is not sent and the status is returned as the result.
 
    Return value:
 
    This function returns an error if one of the parameters is invalid or the
    stream is shutdown or closed, or the status argument is negative. It returns
    one if the status argument is one and the peek is completed successfully. 
*/
EXTERN t_error
stream_poke_end(t_stream stream, t_stream_poke_state * state, t_error status);

/*
 * Name:   stream_set_position
 *
 * Description:
 *
 * This function sets the current position of the stream. For send operations, it 
 * flushes the stream send buffer completely before doing so to ensure that data in the
 * stream buffer is written to the file prior to changing the stream pointer. For
 * receive operations, it clears the stream receive buffer so that subsequent
 * receive operations re-read the stream from the new location.
 *
 * This function is not supported by all protocols. Only protocols supporting random
 * access may be used, such as the file protocol.
 *
 * Parameters:
 *
 * stream   = a stream established using stream_listen, stream_connect or stream_accept.
 * position = a 64-bit integer containing the new relative position. Whether the new
 *            position is relative to the beginning of the stream, current position or
 *            end of the stream depends on the method parameter.
 * method   = whether the position argument is relative to the beginning of the stream
 *            (STREAM_MOVE_FROM_BEGINNING), the current position of the stream
 *            (STREAM_MOVE_FROM_CURRENT), or the end of the stream (STREAM_MOVE_FROM_END).
 *
 * Return value:
 * 
 * Returns the new stream position (relative to the beginning of the stream) on success. 
 * If an error occurs then a negative error code is returned.
 */
EXTERN t_long
stream_set_position(t_stream stream, t_long position, t_move_method method);

/*
 * Name:   stream_get_position
 *
 * Description:
 *
 * This function returns the current position of the stream relative to the beginning. 
 * For send operations, it flushes the stream send buffer completely before doing so to 
 * ensure that data in the stream buffer is written to the file prior to obtaining the 
 * stream pointer.
 *
 * This function is not supported by all protocols. Only protocols supporting random
 * access may be used, such as the file protocol.
 *
 * Parameters:
 *
 * stream = a stream established using stream_listen, stream_connect or stream_accept.
 *
 * Return value:
 * 
 * Returns the current stream position on success. If an error occurs then a negative error
 * code is returned.
 */
EXTERN t_long
stream_get_position(t_stream stream);

/*
 * Name:   stream_get_boolean_property
 *
 * Description:
 *
 * This function returns the value of the specified boolean properties.
 * This function is optional in a driver. If the driver does not provide
 * it then this function returns -QERR_NOT_SUPPORTED.
 *
 * Parameters:
 *
 * stream         = a stream established using stream_listen, stream_connect or stream_accept.
 * properties     = an array containing the properties to query
 * num_properties = the number of elements in the properties array
 * buffer         = an array into which the property values are stored. It must have the same
 *                  number of elements as the properties array.
 *
 * Return value:
 * 
 * Returns 0 on success. Otherwise it returns a negative error code. If the property is
 * not recognized then it returns -QERR_PROPERTY_NOT_RECOGNIZED.
 */
EXTERN t_error
stream_get_boolean_property(t_stream stream, const t_stream_boolean_property properties[], t_uint num_properties, t_boolean buffer[]);

/*
 * Name:   stream_set_boolean_property
 *
 * Description:
 *
 * This function sets the value of the specified boolean properties.
 * This function is optional in a driver. If the driver does not provide
 * it then this function returns -QERR_NOT_SUPPORTED.
 *
 * Parameters:
 *
 * stream         = a stream established using stream_listen, stream_connect or stream_accept.
 * properties     = an array containing the properties to set
 * num_properties = the number of elements in the properties array
 * buffer         = an array containing the property values. It must have the same
 *                  number of elements as the properties array.
 *
 * Return value:
 * 
 * Returns 0 on success. Otherwise it returns a negative error code. If the property is
 * not recognized then it returns -QERR_PROPERTY_NOT_RECOGNIZED.
 */
EXTERN t_error
stream_set_boolean_property(t_stream stream, const t_stream_boolean_property properties[], t_uint num_properties, const t_boolean buffer[]);

/*
 * Name:   stream_get_integer_property
 *
 * Description:
 *
 * This function returns the value of the specified integer properties.
 *
 * This function is optional in a driver. If the driver does not provide
 * it then this function returns -QERR_NOT_SUPPORTED.
 *
 * Parameters:
 *
 * stream         = a stream established using stream_listen, stream_connect or stream_accept.
 * properties     = an array containing the properties to query
 * num_properties = the number of elements in the properties array
 * buffer         = an array into which the property values are stored. It must have the same
 *                  number of elements as the properties array.
 *
 * Return value:
 * 
 * Returns 0 on success. Otherwise it returns a negative error code. If the property is
 * not recognized then it returns -QERR_PROPERTY_NOT_RECOGNIZED.
 */
EXTERN t_error
stream_get_integer_property(t_stream stream, const t_stream_integer_property properties[], t_uint num_properties, t_int buffer[]);

/*
 * Name:   stream_set_integer_property
 *
 * Description:
 *
 * This function sets the value of the specified integer properties.
 *
 * This function is optional in a driver. If the driver does not provide
 * it then this function returns -QERR_NOT_SUPPORTED.
 *
 * Parameters:
 *
 * stream         = a stream established using stream_listen, stream_connect or stream_accept.
 * properties     = an array containing the properties to set
 * num_properties = the number of elements in the properties array
 * buffer         = an array containing the property values. It must have the same
 *                  number of elements as the properties array.
 *
 * Return value:
 * 
 * Returns 0 on success. Otherwise it returns a negative error code. If the property is
 * not recognized then it returns -QERR_PROPERTY_NOT_RECOGNIZED.
 */
EXTERN t_error
stream_set_integer_property(t_stream stream, const t_stream_integer_property properties[], t_uint num_properties, const t_int buffer[]);

/*
 * Name:   stream_get_double_property
 *
 * Description:
 *
 * This function returns the value of the specified double properties.
 *
 * This function is optional in a driver. If the driver does not provide
 * it then this function returns -QERR_NOT_SUPPORTED.
 *
 * Parameters:
 *
 * stream         = a stream established using stream_listen, stream_connect or stream_accept.
 * properties     = an array containing the properties to query
 * num_properties = the number of elements in the properties array
 * buffer         = an array into which the property values are stored. It must have the same
 *                  number of elements as the properties array.
 *
 * Return value:
 * 
 * Returns 0 on success. Otherwise it returns a negative error code. If the property is
 * not recognized then it returns -QERR_PROPERTY_NOT_RECOGNIZED.
 */
EXTERN t_error
stream_get_double_property(t_stream stream, const t_stream_double_property properties[], t_uint num_properties, t_double buffer[]);

/*
 * Name:   stream_set_double_property
 *
 * Description:
 *
 * This function sets the value of the specified double properties.
 *
 * This function is optional in a driver. If the driver does not provide
 * it then this function returns -QERR_NOT_SUPPORTED.
 *
 * Parameters:
 *
 * stream         = a stream established using stream_listen, stream_connect or stream_accept.
 * properties     = an array containing the properties to set
 * num_properties = the number of elements in the properties array
 * buffer         = an array containing the property values. It must have the same
 *                  number of elements as the properties array.
 *
 * Return value:
 * 
 * Returns 0 on success. Otherwise it returns a negative error code. If the property is
 * not recognized then it returns -QERR_PROPERTY_NOT_RECOGNIZED.
 */
EXTERN t_error
stream_set_double_property(t_stream stream, const t_stream_double_property properties[], t_uint num_properties, const t_double buffer[]);

/*
 * Name:   stream_get_string_property
 *
 * Description:
 *
 * This function returns the value of the specified string property.
 *
 * This function is optional in a driver. If the driver does not provide
 * it then this function returns -QERR_NOT_SUPPORTED.
 *
 * Parameters:
 *
 * stream         = a stream established using stream_listen, stream_connect or stream_accept.
 * property_code  = the property to query
 * buffer         = a character array into which the property value is stored. It must be at
 *                  least buffer_size characters in length.
 * buffer_size    = the size of the buffer in characters.
 *
 * Return value:
 * 
 * Returns 0 on success. Otherwise it returns a negative error code. If the property is
 * not recognized then it returns -QERR_PROPERTY_NOT_RECOGNIZED.
 */
EXTERN t_error
stream_get_string_property(t_stream stream, const t_stream_string_property property_code, char * buffer, size_t buffer_size);

/*
 * Name:   stream_set_string_property
 *
 * Description:
 *
 * This function sets the value of the specified string property.
 *
 * This function is optional in a driver. If the driver does not provide
 * it then this function returns -QERR_NOT_SUPPORTED.
 *
 * Parameters:
 *
 * stream         = a stream established using stream_listen, stream_connect or stream_accept.
 * property_code  = the property to set
 * buffer         = a character array into which the property value is stored. It must be at
 *                  least buffer_size characters in length.
 * buffer_size    = the size of the buffer in characters.
 *
 * Return value:
 * 
 * Returns 0 on success. Otherwise it returns a negative error code. If the property is
 * not recognized then it returns -QERR_PROPERTY_NOT_RECOGNIZED.
 */
EXTERN t_error
stream_set_string_property(t_stream stream, const t_stream_string_property property_code, const char * buffer, size_t buffer_size);

/*
    Name:   stream_shutdown

    Description:

    This function shuts down send and/or receives in preparation for
    closing the stream.

    Parameters:

    stream = a stream established using stream_listen, stream_connect or stream_accept.

    Return value:

    Returns 0 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_error
stream_shutdown(t_stream stream);

/*
    Name:   stream_close

    Description:

    This function closes the stream. All resources associated with
    the stream are freed.

    Parameters:

    stream  = a stream established using stream_listen, stream_connect or stream_accept.

    Return value:

    Returns 0 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_error
stream_close(t_stream stream);

/*
    Name:   stream_close_all

    Description:

    This function closes all streams established using stream_listen, stream_connect
    or stream_accept. The list of streams is maintained in an internal database which
    is only valid within the context of the module which links with the
    quanser_communications static link library. Hence, the stream API cannot be called
    from an RTDLL unless the RTDLL is only used by a single RTSSS process. Also, if
    two separate modules in the same process link with the quanser_communications
    library then the stream functions will only recognize a stream as valid if it
    was established by the same module.

    Parameters:

    none

    Return value:

    Returns 0 on success. If an error occurs then a negative error code is returned.
*/
EXTERN t_error
stream_close_all(void);

#if defined(_DEBUG)

EXTERN void
stream_dump(t_stream stream);

EXTERN void
stream_dump_all(void);

#endif

#endif
