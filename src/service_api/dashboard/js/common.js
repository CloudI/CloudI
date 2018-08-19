//-*-Mode:javascript;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=javascript fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

// parse a CloudI Service API response using JSON
function parseResponse(data, callback) {
    var response;
    try {
        response = JSON.parse(data);
    }
    catch (e) {
        console.log(`received invalid JSON "${data}"`);
        return;
    }
    if (response.success == false) {
        console.log(`CloudI Service API error "${response.error}"`);
        return;
    }
    if (typeof callback !== 'undefined') {
        callback(response);
    }
}

// common javascript example http request function
function httpRequest(method, url) {
    // function createCORSRequest(method, url) source code
    // commonly used in javascript examples
    var xhr = new XMLHttpRequest();
    if ('withCredentials' in xhr) {
        // Check if the XMLHttpRequest object
        // has a "withCredentials" property.
        // "withCredentials" only exists on XMLHTTPRequest2 objects.
        xhr.open(method, url, true);
    } else if (typeof XDomainRequest !== 'undefined') {
        // Otherwise, check if XDomainRequest.
        // XDomainRequest only exists in IE, and is IE's way of
        // making CORS requests.
        xhr = new XDomainRequest();
        xhr.open(method, url);
    } else {
        // Otherwise, CORS is not supported by the browser.
        xhr = null;
        // throw an error if the browser is not supported
        throw new Error('createCORSRequest failed!');
    }
    return xhr;
}
