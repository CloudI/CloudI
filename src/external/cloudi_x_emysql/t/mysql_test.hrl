-define(DROP_TABLES(PoolId), begin
	ShowTables = emysql:execute(PoolId, "SHOW TABLES"),
	[emysql:execute(PoolId, <<"DROP TABLE `", Table/binary, "`">>) || [Table] <- ShowTables#result_packet.rows]
end).