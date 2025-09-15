<script lang=ts>

import PocketBase from 'pocketbase';
import { redirect } from '@sveltejs/kit';

let pb = new PocketBase("http://127.0.0.1:8090");

export async function load({url}:{ url: URL }) {
  const code = url.searchParams.get('code');
  if (!code) return { error: 'Missing code' };



  // Finish OAuth2 login
  await pb.collection('users').authWithOAuth2Code(
    'gitlab',
    code,
    'http://localhost:5173/auth/callback' // must exactly match GitLab
  );

  throw redirect(302, '/'); // redirect home after login
}

</script>
