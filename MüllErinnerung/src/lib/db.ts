import PocketBase from 'pocketbase'

const pb = new PocketBase('http://127.0.0.1:8090')

export function isLoggedIn() {
    return pb.authStore.isValid;
}
// TODO
export async function login(username: string, password: string){
    try{
        if(isLoggedIn()){
            await pb.collection('users').authRefresh();
            return;
        }
    } catch(e){
        console.error("Error refreshing Auth:")
    }
    return await pb.collection('users').authWithOAuth2
}