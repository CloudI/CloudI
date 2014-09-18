# Test Setup

As the user root

    mysql
    > CREATE DATABASE cloudi_tests;
    > CREATE USER 'cloudi_tests'@'localhost' IDENTIFIED BY 'cloudi_tests';
    > GRANT ALL ON cloudi_tests.* TO cloudi_tests;

