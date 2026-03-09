#pragma once

#ifdef WITH_HTTP
#include <curl/curl.h>
#include "scheme_engine.h"
#include <string>


void initialize_curl();
void shutdown_curl();

CURL* get_curl_handle();

struct CurlInfo {
  bool complete;
  std::string data;

};

size_t writeback(void *buffer, size_t size, size_t nmemb, void *userp);

SCHEME_FUNC(scm_curl_get_sync)
{
    CURL* handle = get_curl_handle();
    curl_easy_setopt(handle, CURLOPT_URL, s7_string(s7_car(args)));

    CurlInfo future;
    curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION,writeback);

    curl_easy_setopt(handle, CURLOPT_WRITEDATA, &future);
    auto success = curl_easy_perform(handle);
    //like spinlock basically until the response comes back
    while (!future.complete) {}

    return s7_make_string(get_scheme(), future.data.c_str());
}

#endif
