(*
   Get Google user profile, including email address.

   Reference: https://developers.google.com/+/api/latest/people/get

*)

open Lwt
open Google_profile_t

let get_profile_gen with_token id =
  let url = Google_api_util.make_uri ~path:"/plus/v1/people/me" () in
  with_token id (fun access_token ->
    Util_http_client.get
      ~headers:[Google_auth.auth_header access_token]
      url
  ) >>= fun (status, headers, body) ->
  match status with
  | `OK -> return (Google_profile_j.profile_of_string body)
  | `Not_found -> Http_exn.not_found "Google profile not found"
  | _ -> Http_exn.bad_request "Cannot access Google profile"

(*
   Get profile with an access_token, which is needed
   in the Google login process in order to retrieve the user's email address
   when we don't know the Esper uid yet.
*)
let try_get_profile access_token =
  let with_token id http_call =
    let access_token = id in
    http_call access_token
  in
  get_profile_gen with_token access_token

(*
   We can't do the following here because of dependencies:

let get_profile uid =
  get_profile_gen User_account.google_http uid
*)

let extract_email_address profile =
  match List.filter (fun x -> x.type_ = "account") profile.emails with
  | [] ->
      Http_exn.forbidden
        "No account email address found in Google+ profile. \
         Missing permission for email scope?"
  | x :: _ -> x.value

let try_get_google_email_address access_token =
  try_get_profile access_token >>= fun profile ->
  return (extract_email_address profile)
