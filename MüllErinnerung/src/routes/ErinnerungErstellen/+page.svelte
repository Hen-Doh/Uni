<script>
        import { json } from '@sveltejs/kit';
    import PocketBase from 'pocketbase';
    
    const pb = new PocketBase('http://127.0.0.1:8090');
    let record=$state("record")
    //async checkIfUserHasReminder().
    async function request(){
            // example create data
        const data = {
            "Pickup": "2022-01-01 10:00:00.123Z",
            "Typ": "Bio",
            "Reminder": "2022-01-01 10:00:00.123Z"
        };

        record = await pb.collection('Erinnerung').create(data);
        console.log(record)
        console.log(JSON.stringify(record, null, 2)); // Pretty-printed JSON output

    }
</script>

<button onclick={request}>recordkeeping</button>
<h2>{record}</h2>