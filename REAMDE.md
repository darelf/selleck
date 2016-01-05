# Selleck

### Overview

Serve up html from mustache templates and provided data. Data is supplied in the
body of the request as `json`. Mustache implementation comes from
[bbmustache](https://github.com/soranoba/bbmustache) and web framework is
[cowboy](https://ninenines.eu/)

Kind of an experiment. Doesn't handle every case. Specifically, non-existant files
cause a 500 error rather than a 404. ( Something to add in the future )

    make

or

    make run

### Configuration

The port that this service runs on is controlled in the `sys.config` file.
The default in the repository is `5555`.

### Templates

Templates are stored in the `priv/templates` directory, and it expects there
to be a sub-directory where each "product" stores templates. There is a test
template that exists in `priv/templates/test/test.mustache` and with the
default configuration you can try it out using:

    curl -i http://localhost:5555/test/test -d '{"title": "My Title"}'

### Contributions

Always welcome! PRs, issues, whatever.

### Contact

Twitter @darelf
