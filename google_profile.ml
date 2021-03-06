(*
   Get Google user profile, including email address.

   Reference: https://developers.google.com/+/api/latest/people/get

*)

open Lwt
open Google_profile_t

let call_get_profile access_token =
  let url = Google_api_util.make_uri "/plus/v1/people/me" in
  Util_http_client.get
    ~headers:[Google_auth.auth_header access_token]
    url

let get_profile with_token =
  with_token call_get_profile >>= fun (status, headers, body) ->
  match status with
  | `OK -> return (Google_profile_j.profile_of_string body)
  | `Not_found ->
      Http_exn.not_found `Google_profile_not_found "Google profile not found"
  | _ ->
      failwith "Cannot access Google profile"

let extract_email_address profile =
  let email_l =
    List.filter (fun (x : Google_profile_t.email_info) -> x.type_ = "account")
      profile.emails
  in
  match email_l with
  | [] ->
      Http_exn.forbidden
        `Google_email_not_found
        "No account email address found in Google+ profile. \
         Missing permission for email scope?"
  | x :: _ -> x.value

let get_google_email_address with_token =
  get_profile with_token >>= fun profile ->
  return (extract_email_address profile)
