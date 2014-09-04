(*
   Get Google user profile, including email address.

   Reference: https://developers.google.com/+/api/latest/people/get

*)

open Lwt
open Google_profile_t

let get_profile access_token =
  let url = Google_api_util.make_uri ~path:"/plus/v1/people/me" () in
  Util_http_client.get
    ~headers:[Google_auth.auth_header access_token]
    url
  >>= fun (status, headers, body) ->
  match status with
  | `OK -> return (`Result (Google_profile_j.profile_of_string body))
  | `Unauthorized -> return `Retry
  | `Not_found -> Http_exn.not_found "Google profile not found"
  | _ -> Http_exn.bad_request "Cannot access Google profile"

let extract_email_address profile =
  match List.filter (fun x -> x.type_ = "account") profile.emails with
  | [] ->
      Http_exn.forbidden
        "No account email address found in Google+ profile. \
         Missing permission for email scope?"
  | x :: _ -> x.value

let get_google_email_address access_token =
  get_profile access_token >>= fun opt_profile ->
  return (Http_result.map opt_profile extract_email_address)
