# Test Setup

As the postgres user

    psql -h 127.0.0.1 -U postgres
    > CREATE USER cloudi_tests WITH PASSWORD 'cloudi_tests';
    > CREATE DATABASE cloudi_tests;
    > GRANT ALL PRIVILEGES ON DATABASE cloudi_tests to cloudi_tests;

