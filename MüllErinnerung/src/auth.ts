import { SvelteKitAuth } from "@auth/sveltekit";
import GitLab from "@auth/sveltekit/providers/gitlab";
//TODO make it save
import client_data from '$lib/secret.json' 


export const { handle, signIn, signOut } = SvelteKitAuth({
  providers: [
    GitLab({
      clientId: client_data.ApplicationID,
      clientSecret: client_data.Secret!,
      issuer: "https://gitlab.gwdg.de",
      token: "https://gitlab.gwdg.de/oauth/token",
      userinfo: "https://gitlab.gwdg.de/api/v4/user",
      authorization: {
        url: "https://gitlab.gwdg.de/oauth/authorize",
        params: {
          scope: "read_user"  // your GitLab-approved scope
        }
      }

    })
  ],
  //TODO change secret
  secret: "gloas-ae9d9f9daeb7baadc5785c53ead7d31302ec67531de7f5d4ee78d31d543ae81f" // replace with something strong in production
});