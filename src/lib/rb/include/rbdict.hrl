-type rbdict() :: 'empty' |
                  {'b', 'empty', any(), any(), 'empty'} |
                  {'b',
                   'empty',
                   any(), any(),
                   {'r', 'empty', any(), any(), 'empty'}} |
                  {'b',
                   {'r', 'empty', any(), any(), 'empty'},
                   any(), any(),
                   'empty'} |
                  {'b',
                   {'r', 'empty', any(), any(), 'empty'},
                   any(), any(),
                   {'r', 'empty', any(), any(), 'empty'}} |
                  {'b',
                   {'r' | 'b', tuple(), any(), any(), tuple()},
                   any(), any(),
                   {'r' | 'b', tuple(), any(), any(), tuple()}}.

