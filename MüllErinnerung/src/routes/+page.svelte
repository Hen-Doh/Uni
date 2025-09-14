
<script>
  import { page } from "$app/stores";
  import PocketBase from 'pocketbase';

  export const pb = new PocketBase('http://127.0.0.1:8090');
  const loginWithGitLab = async () => {
    // This opens the OAuth login in a new window
    const authUrl = pb.collection('users').authWithOAuthUrl('gitlab', 'http://localhost:5173/oauth/callback');
    window.location.href = authUrl;
  };

</script>

<button on:click={loginWithGitLab}>Login with Gitlab</button>

<h1>Hello World</h1>