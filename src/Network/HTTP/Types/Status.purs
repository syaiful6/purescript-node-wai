module Network.HTTP.Types.Status
  ( Status(..)
  , Redirection
  , status2Redirection, number2Redirection, redirection2Status
  , status2Number
  , number2Status
  , status0
  , status100, status101
  , status200, status201, status202, status203, status204, status205, status206
  , status300, status301, status302, status303, status304, status305, status307
  , status400, status401, status402, status403, status404, status405, status406, status408, status410
  , status411, status412, status413, status414, status415, status416, status417
  , status500, status501, status502, status503, status504, status505
  ) where

import Prelude

import Data.Maybe (Maybe(..))


data Status
  = NoStatus
  | Accepted
  | BadGateway
  | BadRequest
  | Continue
  | Created
  | ExpectationFailed
  | Forbidden
  | Found
  | GatewayTimeout
  | Gone
  | HTTPVersionNotSupported
  | InternalServerError
  | LengthRequired
  | MethodNotAllowed
  | MovedPermanently
  | MultipleChoices
  | NoContent
  | NonAuthoritativeInformation
  | NotAcceptable
  | NotFound
  | NotImplemented
  | NotModified
  | Ok
  | PartialContent
  | PaymentRequired
  | PreconditionFailed
  | ProxyAuthenticationRequired
  | RequestedRangeNotSatisfiable
  | RequestEntityTooLarge
  | RequestTimeout
  | RequestURITooLong
  | ResetContent
  | SeeOther
  | ServiceUnavailable
  | SwitchingProtocols
  | TemporaryRedirect
  | Unauthorized
  | UnsupportedMediaType
  | UseProxy

newtype Redirection = Redirection Status

derive newtype instance eqRedirection :: Eq Redirection
derive newtype instance ordRedirection :: Ord Redirection

derive instance eqStatus :: Eq Status
derive instance ordStatus :: Ord Status

instance showStatus :: Show Status where
  show NoStatus                     = "NoStatus"
  show Accepted                     = "Accepted"
  show BadGateway                   = "BadGateway"
  show BadRequest                   = "BadRequest"
  show Continue                     = "Continue"
  show Created                      = "Created"
  show ExpectationFailed            = "ExpectationFailed"
  show Forbidden                    = "Forbidden"
  show Found                        = "Found"
  show GatewayTimeout               = "GatewayTimeout"
  show Gone                         = "Gone"
  show HTTPVersionNotSupported      = "HTTPVersionNotSupported"
  show InternalServerError          = "InternalServerError"
  show LengthRequired               = "LengthRequired"
  show MethodNotAllowed             = "MethodNotAllowed"
  show MovedPermanently             = "MovedPermanently"
  show MultipleChoices              = "MultipleChoices"
  show NoContent                    = "NoContent"
  show NonAuthoritativeInformation  = "NonAuthoritativeInformation"
  show NotAcceptable                = "NotAcceptable"
  show NotFound                     = "NotFound"
  show NotImplemented               = "NotImplemented"
  show NotModified                  = "NotModified"
  show Ok                           = "Ok"
  show PartialContent               = "PartialContent"
  show PaymentRequired              = "PaymentRequired"
  show PreconditionFailed           = "PreconditionFailed"
  show ProxyAuthenticationRequired  = "ProxyAuthenticationRequired"
  show RequestedRangeNotSatisfiable = "RequestedRangeNotSatisfiable"
  show RequestEntityTooLarge        = "RequestEntityTooLarge"
  show RequestTimeout               = "RequestTimeout"
  show RequestURITooLong            = "RequestURITooLong"
  show ResetContent                 = "ResetContent"
  show SeeOther                     = "SeeOther"
  show ServiceUnavailable           = "ServiceUnavailable"
  show SwitchingProtocols           = "SwitchingProtocols"
  show TemporaryRedirect            = "TemporaryRedirect"
  show Unauthorized                 = "Unauthorized"
  show UnsupportedMediaType         = "UnsupportedMediaType"
  show UseProxy                     = "UseProxy"

instance showRedirection :: Show Redirection where
  show (Redirection s) = show s

status2Redirection :: Status -> Maybe Redirection
status2Redirection MultipleChoices              = Just (Redirection MultipleChoices)
status2Redirection MovedPermanently             = Just (Redirection MovedPermanently)
status2Redirection Found                        = Just (Redirection Found)
status2Redirection SeeOther                     = Just (Redirection SeeOther)
status2Redirection NotModified                  = Just (Redirection NotModified)
status2Redirection UseProxy                     = Just (Redirection UseProxy)
status2Redirection TemporaryRedirect            = Just (Redirection TemporaryRedirect)
status2Redirection _                            = Nothing

redirection2Status :: Redirection -> Status
redirection2Status (Redirection s) = s

number2Redirection :: Int -> Maybe Redirection
number2Redirection = number2Status >=> status2Redirection

status2Number :: Status -> Int
-- 0 Default status for unsent requests
status2Number NoStatus                     = 0
-- 100 Informational
status2Number Continue                     = 100
status2Number SwitchingProtocols           = 101
-- 200 Successful
status2Number Ok                           = 200
status2Number Created                      = 201
status2Number Accepted                     = 202
status2Number NonAuthoritativeInformation  = 203
status2Number NoContent                    = 204
status2Number ResetContent                 = 205
status2Number PartialContent               = 206
-- 300 Redirection
status2Number MultipleChoices              = 300
status2Number MovedPermanently             = 301
status2Number Found                        = 302
status2Number SeeOther                     = 303
status2Number NotModified                  = 304
status2Number UseProxy                     = 305
status2Number TemporaryRedirect            = 307
-- 400 Client Error
status2Number BadRequest                   = 400
status2Number Unauthorized                 = 401
status2Number PaymentRequired              = 402
status2Number Forbidden                    = 403
status2Number NotFound                     = 404
status2Number MethodNotAllowed             = 405
status2Number NotAcceptable                = 406
status2Number ProxyAuthenticationRequired  = 407
status2Number RequestTimeout               = 408
status2Number Gone                         = 410
status2Number LengthRequired               = 411
status2Number PreconditionFailed           = 412
status2Number RequestEntityTooLarge        = 413
status2Number RequestURITooLong            = 414
status2Number UnsupportedMediaType         = 415
status2Number RequestedRangeNotSatisfiable = 416
status2Number ExpectationFailed            = 417
-- 500 Server Error
status2Number InternalServerError          = 500
status2Number NotImplemented               = 501
status2Number BadGateway                   = 502
status2Number ServiceUnavailable           = 503
status2Number GatewayTimeout               = 504
status2Number HTTPVersionNotSupported      = 505

number2Status :: Int -> Maybe Status
-- 0 Default status for unsent requests
number2Status 0   = Just NoStatus
-- 100 Informational
number2Status 100 = Just Continue
number2Status 101 = Just SwitchingProtocols
-- 200 Successful
number2Status 200 = Just Ok
number2Status 201 = Just Created
number2Status 202 = Just Accepted
number2Status 203 = Just NonAuthoritativeInformation
number2Status 204 = Just NoContent
number2Status 205 = Just ResetContent
number2Status 206 = Just PartialContent
-- 300 Redirection
number2Status 300 = Just MultipleChoices
number2Status 301 = Just MovedPermanently
number2Status 302 = Just Found
number2Status 303 = Just SeeOther
number2Status 304 = Just NotModified
number2Status 305 = Just UseProxy
number2Status 307 = Just TemporaryRedirect
-- 400 Client Error
number2Status 400 = Just BadRequest
number2Status 401 = Just Unauthorized
number2Status 402 = Just PaymentRequired
number2Status 403 = Just Forbidden
number2Status 404 = Just NotFound
number2Status 405 = Just MethodNotAllowed
number2Status 406 = Just NotAcceptable
number2Status 407 = Just ProxyAuthenticationRequired
number2Status 408 = Just RequestTimeout
number2Status 410 = Just Gone
number2Status 411 = Just LengthRequired
number2Status 412 = Just PreconditionFailed
number2Status 413 = Just RequestEntityTooLarge
number2Status 414 = Just RequestURITooLong
number2Status 415 = Just UnsupportedMediaType
number2Status 416 = Just RequestedRangeNotSatisfiable
number2Status 417 = Just ExpectationFailed
-- 500 Server Error
number2Status 500 = Just InternalServerError
number2Status 501 = Just NotImplemented
number2Status 502 = Just BadGateway
number2Status 503 = Just ServiceUnavailable
number2Status 504 = Just GatewayTimeout
number2Status 505 = Just HTTPVersionNotSupported
number2Status _   = Nothing

status0 :: Status
status0   = NoStatus

status100 :: Status
status100 = Continue

status101 :: Status
status101 = SwitchingProtocols

status200 :: Status
status200 = Ok

status201 :: Status
status201 = Created

status202 :: Status
status202 = Accepted

status203 :: Status
status203 = NonAuthoritativeInformation

status204 :: Status
status204 = NoContent

status205 :: Status
status205 = ResetContent

status206 :: Status
status206 = PartialContent

status300 :: Status
status300 = MultipleChoices

status301 :: Status
status301 = MovedPermanently

status302 :: Status
status302 = Found

status303 :: Status
status303 = SeeOther

status304 :: Status
status304 = NotModified

status305 :: Status
status305 = UseProxy

status307 :: Status
status307 = TemporaryRedirect

status400 :: Status
status400 = BadRequest

status401 :: Status
status401 = Unauthorized

status402 :: Status
status402 = PaymentRequired

status403 :: Status
status403 = Forbidden

status404 :: Status
status404 = NotFound

status405 :: Status
status405 = MethodNotAllowed

status406 :: Status
status406 = NotAcceptable

status407 :: Status
status407 = ProxyAuthenticationRequired

status408 :: Status
status408 = RequestTimeout

status410 :: Status
status410 = Gone

status411 :: Status
status411 = LengthRequired

status412 :: Status
status412 = PreconditionFailed

status413 :: Status
status413 = RequestEntityTooLarge

status414 :: Status
status414 = RequestURITooLong

status415 :: Status
status415 = UnsupportedMediaType

status416 :: Status
status416 = RequestedRangeNotSatisfiable

status417 :: Status
status417 = ExpectationFailed

status500 :: Status
status500 = InternalServerError

status501 :: Status
status501 = NotImplemented

status502 :: Status
status502 = BadGateway

status503 :: Status
status503 = ServiceUnavailable

status504 :: Status
status504 = GatewayTimeout

status505 :: Status
status505 = HTTPVersionNotSupported
