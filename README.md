# Blogpot

An example application integrating Polysemy with Servant.

## Database
Have a PostgreSQL `blogpot` database running on your machine.

## API

### GET /blogposts
Fetch all blogposts.
You can pass `id` query parameter to fetch only one blogpost.

### POST /blogposts
Add a new blogpost to the database.
Body should be JSON with `"blogpostName"` and `"blogpostBody"` string fields.

### DELETE /blogposts
Delete a blogpost with the given id.
Body should be JSON string with the id.

### GET /healthcheck
Check if the server is running.
