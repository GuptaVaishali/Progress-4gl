
/*------------------------------------------------------------------------
    File        : HTTPStatusCodes.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : GMason
    Created     : Thu Aug 06 13:39:22 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */

/* Infomation */
&GLOBAL-DEFINE  HTTP-Continue           100
&GLOBAL-DEFINE  HTTP-SwitchingProtocal  101
&GLOBAL-DEFINE  HTTP-Processing         102
&GLOBAL-DEFINE  HTTP-EarlyHints         103

/* Successful Responses */
&GLOBAL-DEFINE  HTTP-OK                             200
&GLOBAL-DEFINE  HTTP-Created                        201
&GLOBAL-DEFINE  HTTP-Accepted                       202
&GLOBAL-DEFINE  HTTP-NonAuthoritativeInformation    203
&GLOBAL-DEFINE  HTTP-NoContent                      204
&GLOBAL-DEFINE  HTTP-ResetContent                   205
&GLOBAL-DEFINE  HTTP-PartialContent                 206
&GLOBAL-DEFINE  HTTP-MultiStatus                    207
&GLOBAL-DEFINE  HTTP-AlreadyReported                208
&GLOBAL-DEFINE  HTTP-IMUsed                         226

/* Redirection messages */
&GLOBAL-DEFINE  HTTP-MultipleChoice                 300
&GLOBAL-DEFINE  HTTP-MovedPermanently               301
&GLOBAL-DEFINE  HTTP-Found                          302
&GLOBAL-DEFINE  HTTP-SeeOther                       303
&GLOBAL-DEFINE  HTTP-NotModified                    304
&GLOBAL-DEFINE  HTTP-UseProxy                       305
&GLOBAL-DEFINE  HTTP-TemporaryRedirect              307
&GLOBAL-DEFINE  HTTP-PermanentRedirect              308

/* Client error responses */
&GLOBAL-DEFINE  HTTP-BadRequest                     400
&GLOBAL-DEFINE  HTTP-Unauthorized                   401
&GLOBAL-DEFINE  HTTP-Forbidden                      403
&GLOBAL-DEFINE  HTTP-NotFound                       404
&GLOBAL-DEFINE  HTTP-MethodNotAllowed               405
&GLOBAL-DEFINE  HTTP-NotAcceptable                  406
&GLOBAL-DEFINE  HTTP-ProxyAuthenticationRequired    407
&GLOBAL-DEFINE  HTTP-RequestTimeout                 408
&GLOBAL-DEFINE  HTTP-Conflict                       409
&GLOBAL-DEFINE  HTTP-Gone                           410
&GLOBAL-DEFINE  HTTP-LengthRequired                 411
&GLOBAL-DEFINE  HTTP-PreconditionFailed             412
&GLOBAL-DEFINE  HTTP-PayloadTooLarge                413
&GLOBAL-DEFINE  HTTP-URITooLong                     414
&GLOBAL-DEFINE  HTTP-UnsupportedMediaType           415
&GLOBAL-DEFINE  HTTP-RangeNotSatisfiable            416
&GLOBAL-DEFINE  HTTP-ExpectationFailed              417
&GLOBAL-DEFINE  HTTP-MisdirectedRequest             421
&GLOBAL-DEFINE  HTTP-UnprocessableEntity            422
&GLOBAL-DEFINE  HTTP-Locked                         423
&GLOBAL-DEFINE  HTTP-FailedDependency               424
&GLOBAL-DEFINE  HTTP-TooEarly                       425
&GLOBAL-DEFINE  HTTP-ExpectationFailed              417
&GLOBAL-DEFINE  HTTP-UpgradeRequiered               426
&GLOBAL-DEFINE  HTTP-PreconditionRequired           428
&GLOBAL-DEFINE  HTTP-TooManyRequests                429
&GLOBAL-DEFINE  HTTP-RequestHeaderFieldsTooLarge    431
&GLOBAL-DEFINE  HTTP-UnavailableForLegalReasons     451

/* SERVER ERROR resposnes */
&GLOBAL-DEFINE  HTTP-InternalServerError            500
&GLOBAL-DEFINE  HTTP-NotImplemented                 501
&GLOBAL-DEFINE  HTTP-BadGateway                     502
&GLOBAL-DEFINE  HTTP-ServiceUnavailable             503
&GLOBAL-DEFINE  HTTP-GatewayTimeout                 504
&GLOBAL-DEFINE  HTTP-HTTPVersionNotSupported        505
&GLOBAL-DEFINE  HTTP-VariantAlsoNegotiates          506
&GLOBAL-DEFINE  HTTP-InsufficientStorage            507
&GLOBAL-DEFINE  HTTP-LoopDetected                   508
&GLOBAL-DEFINE  HTTP-NotExtended                    510
&GLOBAL-DEFINE  HTTP-NetworkAuthenticationRequired  511
