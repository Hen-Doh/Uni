<script lang=ts>
  import { pb } from '$lib/pb';
  import { redirect } from '@sveltejs/kit';
  import { onMount } from 'svelte';

  onMount(() =>{
    const urlParams = new URLSearchParams(window.location.search)
    const gitCode = urlParams.get('code');
    if (!gitCode) throw { error: 'Missing code' };
    const gitVerifier = sessionStorage.getItem("gitlab_verifier")
    if(gitVerifier==null){
      console.log("gitVerifier ist null damit wird die Auth abgebrochen");
      throw redirect(302, '/')
    }
    // Finish OAuth2 login
    const auth =async () => {
      await pb.collection('users').authWithOAuth2Code(
        'gitlab',
        gitCode,
        gitVerifier,
        'http://localhost:5173/auth/callback' 
      );
    }
    auth().then(()=>window.location.href = "/")
    // wieder zurück zur homepage
  })


</script>
