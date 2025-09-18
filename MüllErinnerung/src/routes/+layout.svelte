<script lang="ts">
	import '../app.css';
	import favicon from '$lib/assets/Müllicon.jpg';
	import { pb } from '$lib/pb';
	let { children } = $props()
	const returnUser=()=>{
		if(pb.authStore.isValid){
			return "ID: "+pb.authStore.record?.id
		} else {
			return "Nicht eingeloggt"
		}
	}
</script>

<svelte:head>
	<link rel="icon" href={favicon}/>
</svelte:head>

<a href="/ErinnerungErstellen" >erstellen</a>

<!-- conten of children page goes here-->
{@render children()}
<input type="text">{returnUser()}<input>
<footer>Impressum</footer>


