<script lang="ts">
	import '../app.css';
	import favicon from '$lib/assets/Müllicon.jpg';

	let { children } = $props()
</script>

<svelte:head>
	<link rel="icon" href={favicon}/>
</svelte:head>

<a href="/ErinnerungErstellen" >erstellen</a>

<!-- conten of children page goes here-->
{@render children()}

<footer>Impressum</footer>


