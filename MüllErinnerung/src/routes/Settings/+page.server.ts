import { redirect } from '@sveltejs/kit';
import PocketBase from 'pocketbase';
import type { Cookies } from '@sveltejs/kit';

//Settings ist nur für eingeloggte user, falls kein user eingeloggt ist -> zum einloggen schicken
export async function load({ cookies }: { cookies: Cookies }) {
  const pb = new PocketBase('http://127.0.0.1:8090');
  pb.authStore.loadFromCookie(cookies.get('pb_auth') || '');

  if (!pb.authStore.isValid) {
    throw redirect(302, '/auth/login');
  }

  return {
    user: pb.authStore.model
  };
}
