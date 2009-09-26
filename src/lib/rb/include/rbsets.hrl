-type rbsets() :: 'empty' |
                  {'b', 'empty', any(), 'empty'} |
                  {'b',
                   'empty',
                   any(),
                   {'r', 'empty', any(), 'empty'}} |
                  {'b',
                   {'r', 'empty', any(), 'empty'},
                   any(),
                   'empty'} |
                  {'b',
                   {'r', 'empty', any(), 'empty'},
                   any(),
                   {'r', 'empty', any(), 'empty'}} |
                  {'b',
                   {'r' | 'b', tuple(), any(), tuple()},
                   any(),
                   {'r' | 'b', tuple(), any(), tuple()}}.

