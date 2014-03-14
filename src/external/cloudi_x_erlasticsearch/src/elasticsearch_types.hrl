-define(elasticsearch_Method_GET, 0).
-define(elasticsearch_Method_PUT, 1).
-define(elasticsearch_Method_POST, 2).
-define(elasticsearch_Method_DELETE, 3).
-define(elasticsearch_Method_HEAD, 4).
-define(elasticsearch_Method_OPTIONS, 5).

-define(elasticsearch_Status_CONT, 100).
-define(elasticsearch_Status_SWITCHING_PROTOCOLS, 101).
-define(elasticsearch_Status_OK, 200).
-define(elasticsearch_Status_CREATED, 201).
-define(elasticsearch_Status_ACCEPTED, 202).
-define(elasticsearch_Status_NON_AUTHORITATIVE_INFORMATION, 203).
-define(elasticsearch_Status_NO_CONTENT, 204).
-define(elasticsearch_Status_RESET_CONTENT, 205).
-define(elasticsearch_Status_PARTIAL_CONTENT, 206).
-define(elasticsearch_Status_MULTI_STATUS, 207).
-define(elasticsearch_Status_MULTIPLE_CHOICES, 300).
-define(elasticsearch_Status_MOVED_PERMANENTLY, 301).
-define(elasticsearch_Status_FOUND, 302).
-define(elasticsearch_Status_SEE_OTHER, 303).
-define(elasticsearch_Status_NOT_MODIFIED, 304).
-define(elasticsearch_Status_USE_PROXY, 305).
-define(elasticsearch_Status_TEMPORARY_REDIRECT, 307).
-define(elasticsearch_Status_BAD_REQUEST, 400).
-define(elasticsearch_Status_UNAUTHORIZED, 401).
-define(elasticsearch_Status_PAYMENT_REQUIRED, 402).
-define(elasticsearch_Status_FORBIDDEN, 403).
-define(elasticsearch_Status_NOT_FOUND, 404).
-define(elasticsearch_Status_METHOD_NOT_ALLOWED, 405).
-define(elasticsearch_Status_NOT_ACCEPTABLE, 406).
-define(elasticsearch_Status_PROXY_AUTHENTICATION, 407).
-define(elasticsearch_Status_REQUEST_TIMEOUT, 408).
-define(elasticsearch_Status_CONFLICT, 409).
-define(elasticsearch_Status_GONE, 410).
-define(elasticsearch_Status_LENGTH_REQUIRED, 411).
-define(elasticsearch_Status_PRECONDITION_FAILED, 412).
-define(elasticsearch_Status_REQUEST_ENTITY_TOO_LARGE, 413).
-define(elasticsearch_Status_REQUEST_URI_TOO_LONG, 414).
-define(elasticsearch_Status_UNSUPPORTED_MEDIA_TYPE, 415).
-define(elasticsearch_Status_REQUESTED_RANGE_NOT_SATISFIED, 416).
-define(elasticsearch_Status_EXPECTATION_FAILED, 417).
-define(elasticsearch_Status_UNPROCESSABLE_ENTITY, 422).
-define(elasticsearch_Status_LOCKED, 423).
-define(elasticsearch_Status_FAILED_DEPENDENCY, 424).
-define(elasticsearch_Status_INTERNAL_SERVER_ERROR, 500).
-define(elasticsearch_Status_NOT_IMPLEMENTED, 501).
-define(elasticsearch_Status_BAD_GATEWAY, 502).
-define(elasticsearch_Status_SERVICE_UNAVAILABLE, 503).
-define(elasticsearch_Status_GATEWAY_TIMEOUT, 504).
-define(elasticsearch_Status_INSUFFICIENT_STORAGE, 506).

%% struct restRequest

-record(restRequest, {method                    :: integer(),
                      uri                       :: string() | binary(),
                      parameters = dict:new()   :: dict(),
                      headers = dict:new()      :: dict(),
                      body = <<"">>             :: string() | binary()
                     }).

%% struct restResponse

-record(restResponse, {status :: integer(),
                       headers :: dict(),
                       body :: string() | binary()
                      }).
