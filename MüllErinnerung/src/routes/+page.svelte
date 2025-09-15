
<script lang=ts>
	import type { AuthProviderInfo } from "pocketbase";
  import PocketBase from 'pocketbase';

  export const pb = new PocketBase('http://127.0.0.1:8090');
  let providers:AuthProviderInfo[] = []
  const loadProviders = async () => {
    const methods = await pb.collection('users').listAuthMethods();
    providers = methods.oauth2.providers;
  };
  const loginWith = (provider:AuthProviderInfo) => {
    sessionStorage.setItem('gitlab_verifier', gitlab.codeVerifier);
    window.location.href = provider.authURL+ ' http://localhost:5173/auth/callback ';
  };

  loadProviders();
  console.log(providers[0])

</script>

{#each providers as provider}
  <button on:click={() => loginWith(provider)}>
    Login with {provider.name}
  </button>
{/each}
<h1>Hello World</h1>