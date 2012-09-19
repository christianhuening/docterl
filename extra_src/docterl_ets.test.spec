%{node, server_a, 'server_a@flar.informatik.haw-hamburg.de'}.
%{node, server_b, 'server_b@flar.informatik.haw-hamburg.de'}.
{node, server_a, 'server_a@ubuntu.harper-hall.de'}.
{node, server_b, 'server_b@ubuntu.harper-hall.de'}.

{alias, main, "."}.
%{alias, server_a, "./test_data/server_a"}.
%{alias, server_b, "./test_data/server_b"}.

{init, [server_a, server_b], [{node_start, [{monitor_master, false}]}]}.

%{cases, [server_a], main, docter_ets_SUITE, test_server_a}.
%{cases, [server_b], main, docter_ets_SUITE, test_server_b}.


%{suites, all_nodes, main, [docterl_ets_SUITE]}.
{suites, [server_a], main, [docterl_ets_SUITE]}.
{suites, [server_b], main, [docterl_ets_SUITE]}.

%{skip_cases, [server_a], master, docterl_ets_SUITE, test_server_b, "skipped"}. 
%{skip_cases, [server_b], master, docterl_ets_SUITE, test_server_a, "skipped"}. 