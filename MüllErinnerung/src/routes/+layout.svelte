<script lang="ts">
	import favicon from '$lib/assets/Müllicon.jpg';
	import '../app.css';
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

<div class="testtailwind">
  <button class ="btn btn-primary" >Layout test</button>

  <h1 class="text-4xl font-bold text-blue-600">Tailwind CSS Test inside +layout</h1>
</div>Tailwind
<footer>Impressum</footer>


