#include <iostream>
#include <curl/curl.h>
#include <string>

size_t WriteCallback(void* contents, size_t size, size_t nmemb, void* userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL* curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://127.0.0.1:22555/"); // Default mainnet RPC port
        curl_easy_setopt(curl, CURLOPT_USERPWD, "rpcuser:rpcpassword"); // Set your rpcuser/rpcpassword
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "{\"jsonrpc\":\"1.0\",\"id\":\"curltest\",\"method\":\"getblockcount\",\"params\":[]}");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);

        res = curl_easy_perform(curl);
        if(res != CURLE_OK)
            std::cerr << "RPC request failed: " << curl_easy_strerror(res) << std::endl;
        else
            std::cout << "✅ Current block count: " << readBuffer << std::endl;

        curl_easy_cleanup(curl);
    }
    return 0;
}
