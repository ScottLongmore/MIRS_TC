1)Test to make sure logging plugins can be used:
	a)cd to the project root dir.
	b)Enter: "python pool secret tests/manual_tests/logging_tests/logging_plugin_test.cfg"
	c)Following text should appear:
		Foo! INFO - Launching pool command: secret
		Foo! INFO - Starting secret.
		IT'S A SECRET TO EVERYBODY.
		Foo! INFO - Finishing secret.
		
2)Test to make sure the default logger can have its level changed:
	a)cd to the project root dir.
	b)Enter: "python pool secret tests/manual_tests/logging_tests/default_logger_test.cfg"
	c)Following text should appear:
		INFO:__main__:Set up default logging using basicConfig.
		INFO:__main__:Launching pool command: secret
		INFO:__main__:Starting secret.
		IT'S A SECRET TO EVERYBODY.
		INFO:__main__:Finishing secret.