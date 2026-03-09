#include <curl/curl.h>
#define WITH_HTTP

#ifdef WITH_HTTP


#include "http.h"

CURL* handle;

CURL *get_curl_handle() {
    return handle;
}

size_t writeback(void *buffer, size_t size, size_t nmemb, void *userp) {
                       ((CurlInfo*)userp)->data.append((char*) buffer,size*nmemb);
                       ((CurlInfo*)userp)->complete = true;
                       return size*nmemb;
                     }

void initialize_curl()
{
  curl_global_init(CURL_GLOBAL_ALL);
  handle = curl_easy_init();
}

void shutdown_curl()
{
    curl_easy_cleanup(handle);
}

#endif
