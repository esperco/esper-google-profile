(*
   Get Google user profile, including email address.

   Reference: https://developers.google.com/+/api/latest/people/get

*)

open Lwt
open Google_profile_t

let get_profile access_token =
  Util_http_client.get
    ~headers:[Google_auth.auth_header access_token]
    (Uri.of_string "https://www.googleapis.com/plus/v1/people/me")
  >>= fun (status, headers, body) ->
  return (Google_profile_j.profile_of_string body)

let get_account_email_address access_token =
  get_profile access_token >>= fun profile ->
  match List.filter (fun x -> x.type_ = "account") profile.emails with
  | [] -> failwith "No account email address found in Google+ profile. \
                    Missing permission for email scope?"
  | x :: _ -> return x.value
