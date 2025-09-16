
<script lang=ts>
	import type { AuthProviderInfo } from "pocketbase";
  import PocketBase from 'pocketbase';

  export const pb = new PocketBase('http://127.0.0.1:8090');
  const testAuth = () =>{
    console.log(pb.authStore.isValid)
    
  }
  let providers:AuthProviderInfo[] = []
  const loadProviders = async () => {
    const methods = await pb.collection('users').listAuthMethods();
    providers = methods.oauth2.providers;
  };
  const loginWith = (provider:AuthProviderInfo) => {
    sessionStorage.setItem('gitlab_verifier', provider.codeVerifier);
    window.location.href = provider.authURL+ 'http://localhost:5173/auth/callback ';
  };

  loadProviders()//.then(()=>console.log(providers));

</script>

{#each providers as provider}
  <button onclick={() => loginWith(provider)}>
    Login with {provider.name}
  </button>
{/each}
<button onclick={()=>testAuth()}>test auth</button>
