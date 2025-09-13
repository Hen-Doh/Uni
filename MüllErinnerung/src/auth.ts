import { SvelteKitAuth } from "@auth/sveltekit";
import GitLab from "@auth/core/providers/gitlab";
//TODO make it save
import client_data from '$lib/secret.json' 


export const { handle, signIn, signOut } = SvelteKitAuth({
  providers: [
    GitLab({
      clientId: client_data.ApplicationID,
      clientSecret: client_data.Secret!,
      issuer: "https://gitlab.gwdg.de",
      authorization: "https://gitlab.gwdg.de/oauth/authorize",
      token: "https://gitlab.gwdg.de/oauth/token",
      userinfo: "https://gitlab.gwdg.de/api/v4/user"
    })
  ],
  //TODO change secret
  secret: "a-very-secret-string" // replace with something strong in production
});
