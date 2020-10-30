package sysdes.formapp.http

/**
  * Standard status code
  */
object Status {
  // 1xx information
  case object Continue           extends Status(100, "Continue")
  case object SwitchingProtocols extends Status(101, "Switching Protocols")
  case object Processing         extends Status(102, "Processing")
  case object EarlyHints         extends Status(103, "Early Hints")
  // 2xx success
  case object OK                          extends Status(200, "OK")
  case object Created                     extends Status(201, "Created")
  case object Accepted                    extends Status(202, "Accepted")
  case object NonAuthoritativeInformation extends Status(203, "Non-Authoritative Information")
  case object NoContent                   extends Status(204, "No Content")
  case object ResetContent                extends Status(205, "Reset Content")
  case object PartialContent              extends Status(206, "Partial Content")
  case object MultiStatus                 extends Status(207, "Multi-Status")
  case object AlreadyReported             extends Status(208, "Already Reported")
  case object IMUsed                      extends Status(226, "IM Used")
  // 3xx redirection
  case object MultipleChoices   extends Status(300, "Multiple Choices")
  case object MovedPermanently  extends Status(301, "Moved Permanently")
  case object Found             extends Status(302, "Found")
  case object SeeOther          extends Status(303, "See Other")
  case object NotModified       extends Status(304, "Not Modified")
  case object UseProxy          extends Status(305, "Use Proxy")
  case object SwitchProxy       extends Status(306, "Switch Proxy")
  case object TemporaryRedirect extends Status(307, "Temporary Redirect")
  case object PermanentRedirect extends Status(308, "PermanentRedirect")
  // 4xx client errors
  case object BadRequest                  extends Status(400, "Bad Request")
  case object Unauthorized                extends Status(401, "Unauthorized")
  case object PaymentRequired             extends Status(402, "Payment Required")
  case object Forbidden                   extends Status(403, "Forbidden")
  case object NotFound                    extends Status(404, "Not Found")
  case object MethodNotAllowed            extends Status(405, "Method Not Allowed")
  case object NotAcceptable               extends Status(406, "Not Acceptable")
  case object ProxyAuthenticationRequired extends Status(407, "Proxy Authentication Required")
  case object RequestTimeout              extends Status(408, "Request Timeout")
  case object Conflict                    extends Status(409, "Conflict")
  case object Gone                        extends Status(410, "Gone")
  case object LengthRequired              extends Status(411, "Length Required")
  case object PreconditionFailed          extends Status(412, "Precondition Failed")
  case object PayloadTooLarge             extends Status(413, "Payload Too Large")
  case object URITooLong                  extends Status(414, "URI Too Long")
  case object UnsupportedMediaType        extends Status(415, "Unsupported Media Type")
  case object RangeNotSatisfiable         extends Status(416, "Range Not Satisfiable")
  case object ExpectationFailed           extends Status(417, "Expectation Failed")
  case object Teapot                      extends Status(418, "I'm a teapot")
  case object MisdirectedRequest          extends Status(421, "Misdirected Request")
  case object UnprocessableEntity         extends Status(422, "Unprocessable Entity")
  case object Locked                      extends Status(423, "Locked")
  case object FailedDependency            extends Status(424, "Failed Dependency")
  case object TooEarly                    extends Status(425, "Too Early")
  case object UpgradeRequired             extends Status(426, "Upgrade Required")
  case object PreconditionRequired        extends Status(428, "Precondition Required")
  case object TooManyRequests             extends Status(429, "Too Many Requests")
  case object RequestHeaderFieldsTooLarge extends Status(431, "Request Header Fields Too Large")
  case object UnavailableForLegalReasons  extends Status(451, "Unavailable For Legal Reasons")
  // 5xx server errors
  case object InternalServerError           extends Status(500, "Internal Server Error")
  case object NotImplemented                extends Status(501, "Not Implemented")
  case object BadGateway                    extends Status(502, "Bad Gateway")
  case object ServiceUnavailable            extends Status(503, "Service Unavailable")
  case object GatewayTimeout                extends Status(504, "Gateway Timeout")
  case object HTTPVersionNotSupported       extends Status(505, "HTTP Version Not Supported")
  case object VariantAlsoNegotiates         extends Status(506, "Variant Also Negotiates")
  case object InsufficientStorage           extends Status(507, "Insufficient Storage")
  case object LoopDetected                  extends Status(508, "Loop Detected")
  case object NotExtended                   extends Status(510, "Not Extended")
  case object NetworkAuthenticationRequired extends Status(511, "Network Authentication Required")
}

/**
  * HTTP status
  * @param code Status code
  * @param message The message corresponds to the code
  */
sealed abstract class Status(code: Int, message: String) {
  override def toString: String = s"${code} ${message}"
}
