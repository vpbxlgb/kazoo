/*
Section: WebHooks
Title: WebHooks
Language: en-US
*/

# WebHooks *Event driven HTTP callbacks*
Smee: I've just had an apostrophe.
Captain Hook: I think you mean an epiphany.


## Subscribing to doc events

```json
{
    "name": "Test",
    "uri": "{{SERVER}}",
    "http_verb": "post",
    "hook": "all",
    "retries": "1",
    "custom_data": {
        "event_type": "doc_edited",
        "doc_type": "user"
    },
    "enabled": true,
}
```

`event_type` can be `doc_edited`, `doc_created`, `doc_deleted`