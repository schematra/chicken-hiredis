#include <sys/socket.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

int redisReplyType(redisReply *r) {
    return r->type;
}

const char* redisReplyStr(redisReply *r) {
    return r->str;
}

size_t redisReplyLen(redisReply *r) {
    return r->len;
}

int64_t redisReplyInteger(redisReply *r) {
    return r->integer;
}

double redisReplyDouble(redisReply *r) {
  return r->dval;
}

size_t redisReplyElements(redisReply *r) {
    return r->elements;
}

redisReply* redisReplyElement(redisReply *r, size_t index) {
    return r->element[index];
}

/* -------- non-blocking helpers -------- */

/* Sentinel for rh_try_get_reply errors. */
#define RH_REPLY_ERR ((void *)-1)

redisContext* rh_connect_nonblock(const char *host, int port) {
    return redisConnectNonBlock(host, port);
}

int rh_ctx_fd(redisContext *c)             { return c ? c->fd : -1; }
int rh_ctx_err(redisContext *c)            { return c ? c->err : -1; }
const char* rh_ctx_errstr(redisContext *c) { return c ? c->errstr : ""; }
int rh_ctx_connected(redisContext *c)      { return (c && (c->flags & REDIS_CONNECTED)) ? 1 : 0; }
void rh_free(redisContext *c)              { if (c) redisFree(c); }

/* SO_ERROR on ctx->fd; 0 = no error, else errno-style code. */
int rh_ctx_so_error(redisContext *c) {
    int soerr = 0;
    socklen_t len = sizeof(soerr);
    if (!c || c->fd < 0) return EINVAL;
    if (getsockopt(c->fd, SOL_SOCKET, SO_ERROR, &soerr, &len) < 0) return errno;
    return soerr;
}

/* Encode (rc, done) into one int:
 *   -1 = REDIS_ERR
 *    0 = OK, more to write
 *    1 = OK, obuf fully drained
 */
int rh_buffer_write(redisContext *c) {
    int done = 0;
    if (redisBufferWrite(c, &done) != REDIS_OK) return -1;
    return done ? 1 : 0;
}

/* 0 = OK, -1 = REDIS_ERR */
int rh_buffer_read(redisContext *c) {
    return redisBufferRead(c) == REDIS_OK ? 0 : -1;
}

/* Returns:
 *   valid reply*  -> got a reply (caller owns; free with freeReplyObject)
 *   NULL          -> need more data, caller should loop
 *   (void*)-1     -> REDIS_ERR, caller reads errstr and marks ctx dead
 */
void* rh_try_get_reply(redisContext *c) {
    void *reply = NULL;
    if (redisGetReplyFromReader(c, &reply) != REDIS_OK) return RH_REPLY_ERR;
    return reply;
}

/* Append a command to the context's output buffer. Pure buffer-append
 * (no socket I/O). Walks a Scheme cons-list of strings.
 * Returns 0 on OK, -1 on failure. */
int rh_append_argv(redisContext *c, int argc, C_word arglist) {
    const char **argv = (const char **)malloc(argc * sizeof(char *));
    size_t *argvlen   = (size_t *)malloc(argc * sizeof(size_t));
    if (!argv || !argvlen) { free(argv); free(argvlen); return -1; }
    C_word pair = arglist;
    for (int i = 0; i < argc; ++i) {
        C_word item = C_block_item(pair, 0);
        argv[i]    = C_c_string(item);
        argvlen[i] = C_header_size(item);
        pair = C_block_item(pair, 1);
    }
    int rc = redisAppendCommandArgv(c, argc, argv, argvlen);
    free(argv);
    free(argvlen);
    return rc == REDIS_OK ? 0 : -1;
}
