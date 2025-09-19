<script lang=ts>


    import { pb } from '$lib/pb';
	import { error } from '@sveltejs/kit';
	import type { RecordModel } from 'pocketbase';
	import { onMount } from 'svelte';
    let settings:RecordModel[]=$state([])
    async function getSettings(){
        //nur die Settings des aktuellen users, filter via Pocketbase Regeln
        let settings = await pb.collection('Settings').getFullList()
    }
    async function createSetting(){
        if(!pb.authStore.isValid){throw error(401,"invalid user got to Settings")}
        const data = {
            "Erinnerungs_Vorlauf": 12,
            //TODO alle typen (vielleicht via Enum)
            "Relevante_Typen": [
                "Bio",
                "Papier",
                "Plastik",
                "Restmüll",
                "Grünschnitt",
            ],
            "Benachrichtigungsart": "Email",
            "user": pb.authStore.record?.id
        };

        const record = await pb.collection('Settings').create(data);
    }
    onMount(()=>{
        getSettings().then(()=>console.log(settings))
        if(settings = []){
            createSetting()
        }
    })
</script>

