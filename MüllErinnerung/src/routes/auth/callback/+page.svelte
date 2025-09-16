<script lang=ts>

import PocketBase from 'pocketbase';
import { redirect } from '@sveltejs/kit';

let pb = new PocketBase("http://127.0.0.1:8090");

export async function load({url}:{ url: URL }) {
  const gitCode = url.searchParams.get('code');
  if (!gitCode) return { error: 'Missing code' };
  const gitVerifier = sessionStorage.getItem("gitlab_verifier")
  if(gitVerifier==null){
    console.log("gitVerifier ist null damit wird die Auth abgebrochen");
    throw redirect(302, '/')
  }
  // Finish OAuth2 login
  await pb.collection('users').authWithOAuth2Code(
    'gitlab',
    gitCode,
    gitVerifier,
    'http://localhost:5173/auth/callback' 
);

  throw redirect(302, '/'); // wieder zurück zur homepage
}

</script>
