int redisReplyType(redisReply *r) {
    return r->type;
}

const char* redisReplyStr(redisReply *r) {
    return r->str;
}

int64_t redisReplyInteger(redisReply *r) {
    return r->integer;
}

double redisReplyDouble(redisReply *r) {
  return r->dval;
}

// Get number of elements
size_t redisReplyElements(redisReply *r) {
    return r->elements;
}

// Get i-th redisReply*
redisReply* redisReplyElement(redisReply *r, size_t index) {
    return r->element[index];
}

// to use in pubsub functions
redisReply *getReply(redisContext *ctx) {
    void *reply = NULL;
    if (redisGetReply(ctx, &reply) == REDIS_OK) {
        return (redisReply *)reply;
    }
    return NULL;
}

