-module(coap).

-record('coap.msg' ,{  
   type :: atom() | integer()
  ,tkl :: non_neg_integer()
  ,code :: atom() | integer()
  ,message_id :: binary()
  ,token :: binary()
  ,options = [] :: [any()]
  ,payload :: binary()
}).

-export[  
   add_options/2
  ,decode/1
  ,encode/1
  ,init_header/2
  ,init_header/4
  ,set_payload/2
].

%% exposed methods

-spec add_options(
   OptionsList :: [{OptionType ::atom() ,OptionData :: binary()}]
  ,OrigMsg :: #'coap.msg'{})
->
  ResMsg :: #'coap.msg'{} | 'invalid_options'
.
add_options(OptList, OrigMsg)
  when is_list(OptList) ,is_record(OrigMsg ,'coap.msg')
->
  lists:foldl(
     fun(Opt, AccMsg) ->
       case do_add_option(Opt ,AccMsg) of
         Msg when is_record(Msg ,'coap.msg')  ->
           Msg
         ;
         _ ->
           AccMsg
       end
     end
    ,OrigMsg
    ,OptList
  )
;
add_options(_ ,_) ->
  invalid_options
.

-spec init_header( 
   MsgType :: 'CON' | 'NON' | 'ACK' | 'RST'
  ,Code :: atom() | integer())
->
   ResMsg :: #'coap.msg'{} | 'invalid'
.
init_header(Mtype ,Code) ->
   Mid = binary:encode_unsigned(random:uniform(65535))
  ,Tokn = <<>>
  ,init_header(Mtype ,Code, Mid, Tokn)
.

-spec init_header(
   MsgType :: 'CON' | 'NON' | 'ACK' | 'RST'
  ,Code :: atom() | integer()
  ,Mid :: binary()
  ,Tokn :: binary())
->
  ResMsg :: #'coap.msg'{} | 'invalid'
.
init_header(Mtype ,Code, Mid, Tokn) ->
  case validate_header(Mtype ,Code, Mid, Tokn) of
    true ->      
      #'coap.msg'{
         type = Mtype
        ,tkl = byte_size(Tokn)
        ,code = Code
        ,message_id = Mid
        ,token = Tokn         
      }     
    ;      
    _ ->
      invalid
  end
.

-spec set_payload(Payload :: binary() ,OrigMsg :: #'coap.msg'{}) ->
  ResMsg :: #'coap.msg'{} | invalid_payload
.
set_payload(Payload ,Msg) when is_record(Msg ,'coap.msg') ,is_binary(Payload) ->
  Msg#'coap.msg'{payload = Payload}
;
set_payload(_ ,_) ->
  invalid_payload
.

-spec decode(Bin :: binary()) -> ResMsg :: #'coap.msg'{} | invalid_header.
decode(<<Ver:2 ,Mtype:2 ,Tkl:4 ,Code:8 ,Mid:2/binary ,Tokn:Tkl/binary
  ,Rest/binary>> = _Header)
->
  case Ver =:= 1 andalso validate_header(Mtype ,Code ,Mid ,Tokn) of 
    true ->
      case parse_option_payload(Rest) of             
        {Opts ,Payload} ->
           Aopts = lists:filtermap(
              fun({Opt ,OptData}) ->
                case get_option_desc(Opt) of
                  OptDesc when OptDesc =/= false ->
                    {true ,{OptDesc ,OptData}}
                    
                  ;
                  _ ->
                    false  
                end
              end
             ,Opts
           )      
          ,#'coap.msg'{
              type = get_mtype_desc(Mtype)
             ,tkl = Tkl  
             ,code = get_code_desc(Code)
             ,message_id = Mid
             ,token = Tokn
             ,options = Aopts
             ,payload = Payload
          }
        ; 
        false ->
          invalid_opt_ext 
      end
    ;
    _ ->
      invalid_header     
    %~
  end  
;
decode(_) ->
  invalid
.

-spec encode(Msg :: #'coap.msg'{}) -> binary() | invalid_coap_msg.
encode(Msg) when is_record(Msg ,'coap.msg') ->
   #'coap.msg'{
      type = Type
     ,tkl = Tkl
     ,code = Code
     ,message_id = Mid
     ,token = Tokn
     ,options = Opts
     ,payload = Payload
   } = Msg
  ,Atype = convert_mtype_to_value(Type)
  ,Acode = convert_code_to_value(Code)
  ,Aopts = [{convert_option_to_value(Opt) ,OptData} || {Opt ,OptData} <- Opts]
  ,{_ ,OptExt} = assemble_opt_ext(Aopts) 
  ,case Payload of
     undefined -> 
       <<1:2 ,Atype:2 ,Tkl:4 ,Acode:8 ,Mid/binary ,Tokn/binary ,OptExt/binary>>
     ;
     Payload ->
       <<1:2 ,Atype:2 ,Tkl:4 ,Acode:8 ,Mid/binary ,Tokn/binary ,OptExt/binary
         ,16#ff:8 ,Payload/binary>>
   end
;
encode(_) ->
  invalid_coap_msg
.

%set_accept(ContentFormat ,Msg)
%  when is_integer(ContentFormat) ,is_record(Msg ,'coap.msg')
%->
%  do_add_option({'ACCEPT' ,binary:encode_unsigned(ContentFormat)} ,Msg)
%;
%set_accept(_ ,_) ->
%  invalid
%.

%set_content_format(ContentFormat ,Msg)
%  when is_integer(ContentFormat) ,is_record(Msg ,'coap.msg')
%->
%  do_add_option({'CONTENT_FORMAT' ,binary:encode_unsigned(ContentFormat)} ,Msg)
%;
%set_content_format(_ ,_) ->
%  invalid
%.

%set_uri_path(UriPathList ,Msg) when is_list(UriPathList) ,is_record(Msg ,'coap.msg') ->
%  add_options([{'URI_PATH' ,UriPath} || UriPath <- UriPathList] ,Msg)  
%;
%set_uri_path(_ ,_) ->
%  invalid
%.

%set_uri_query(UriQueryL ,Msg) when is_list(UriQueryL) ,is_record(Msg ,'coap.msg') ->
%   add_options([{'URI_QUERY' ,UriQuery} || UriQuery <- UriQueryL] ,Msg)  
%;
%set_uri_query(_ ,_) ->
%  invalid
%.

%set_observe(Val ,Msg) when is_integer(Val) ,is_record(Msg ,'coap.msg') ->
%  do_add_option({'OBSERVE' ,binary:encode_unsigned(Val)} ,Msg)
%;
%set_observe(_ ,_) ->
%  invalid
%.

%% local methods
convert_mtype_to_value(Mtype) when is_atom(Mtype) ->
  get_mtype_value(Mtype)
;
convert_mtype_to_value(Mtype) ->
  Mtype
.
convert_code_to_value(Code) when is_atom(Code) ->
  get_code_value(Code)
;
convert_code_to_value(Code) ->
  Code
.
convert_option_to_value(Opt) when is_atom(Opt) ->
  get_option_value(Opt)
;
convert_option_to_value(Opt) ->
  Opt
.

do_add_option({OptType, OptData} = Opt, #'coap.msg'{options=Opts} = Msg)
  when is_tuple(Opt) ,is_binary(OptData)
->
  case validate_option_type(OptType) of
    true ->
      Msg#'coap.msg'{options = insert_option({OptType ,OptData} ,Opts)}
    ;
    _ ->
      Msg
  end
;
do_add_option(_ ,_) ->
  invalid_option
.

insert_option(X ,[]) ->
  [X]
;
insert_option({NewOpt, _} = X ,[{Opt, _} = H | T] = L) ->
  case get_option_value(NewOpt) >= get_option_value(Opt) of
     true ->
       [H | insert_option(X ,T)]
     ;
     false ->
       [X | L]
  end  
.

assemble_opt_ext([]) ->
  <<>>
;
assemble_opt_ext(Opts) ->   
  lists:foldl(
     fun({OptVal, OptData} ,{PreOptVal ,Acc}) ->        
        OptLen = byte_size(OptData)
       ,Delta = OptVal - PreOptVal  
       ,case {Delta ,OptLen} of
          {Delta ,OptLen} when Delta < 13 andalso OptLen < 13 ->
             {OptVal ,<<Acc/binary ,Delta:4 ,OptLen:4 ,OptData/binary>>}
          ;
          {Delta ,OptLen} when Delta >= 13 andalso Delta < 269
            andalso OptLen < 13
          ->
             DeltaExt = Delta - 13
            ,{OptVal ,<<Acc/binary ,13:4 ,OptLen:4 ,DeltaExt:8 ,OptData/binary>>}
          ;
          {Delta ,OptLen} when Delta >= 13 andalso OptLen < 13 ->
             DeltaExt = Delta - 13
            ,{OptVal ,<<Acc/binary ,13:4 ,OptLen:4 ,DeltaExt:8 ,OptData/binary>>}
          ;
          {Delta ,OptLen} when Delta < 13 andalso OptLen >= 13
            andalso OptLen < 269
          ->
             LenExt = OptLen - 13
            ,{OptVal ,<<Acc/binary ,Delta:4 ,13:4 ,LenExt:8 ,OptData/binary>>}
          ;  
          {Delta ,OptLen} when Delta >= 13 andalso Delta < 269
            andalso OptLen >= 13  andalso OptLen < 269
          ->
             DeltaExt = Delta - 13
            ,LenExt = OptLen - 13
            ,{OptVal ,<<Acc/binary ,13:4 ,13:4 ,DeltaExt:8 ,LenExt:8
              ,OptData/binary>>}            
       end
     end
  ,{0 ,<<>>}
  ,Opts
  )
.

parse_option_payload(<<>>) ->
  {[] ,<<>>} %% {options, payload}
;
parse_option_payload(Data) ->
  do_parse_options(Data, [])
.

do_parse_options(<<>>, AccOpts) ->
  {AccOpts, <<>>} %% options, no payload
;
do_parse_options(Data ,AccOpts) ->
   <<Delta:4 ,Length:4, Ext/binary>> = Data
  ,case parse_option_delta(Delta ,Length ,Ext) of
     {OptDelta ,OptData ,Rest} ->
        case AccOpts of 
          [] ->
            do_parse_options(Rest ,[{OptDelta ,OptData}])
          ;
          OptAcc ->
             {PreOptValue ,_PreOptData} = lists:last(OptAcc)           
            ,do_parse_options(
               Rest
              ,AccOpts ++ [{OptDelta + PreOptValue ,OptData}])
        end
     ;         
     {payload, Payload} ->
       {AccOpts ,Payload}
     ;  
     invalid ->
       false
  end
.

parse_option_delta(Delta ,Length ,Rest) when Delta =< 12 andalso Length =< 12 ->
   <<Data:Length/binary ,Remain/binary>> = Rest
  ,{Delta, Data ,Remain} 
;
parse_option_delta(Delta ,Length ,Rest) when Delta =:= 13 andalso Length =< 12 ->
   <<ExtDelta:8, Data:Length/binary ,Remain/binary>> = Rest
  ,{13 + ExtDelta ,Data, Remain}
;
parse_option_delta(Delta ,Length ,Rest) when Delta =:= 14 andalso Length =< 12 ->
   <<ExtDelta:16, Data:Length/binary ,Remain/binary>> = Rest
  ,{269 + ExtDelta ,Data, Remain}
;
parse_option_delta(Delta ,Len ,Rest) when Delta =<12 andalso Len =:= 13 ->
   <<ExtLen:8, _/binary>> = Rest
  ,ALen = Len + ExtLen
  ,<<ExtLen:8, Data:ALen/binary ,Remain/binary>> = Rest
  ,{Delta ,Data, Remain}
;
parse_option_delta(Delta ,Len ,Rest) when Delta =<12 andalso Len =:= 14 ->
   <<ExtLen:16, _/binary>> = Rest
  ,ALen = Len + ExtLen 
  ,<<ExtLen:8, Data:ALen/binary ,Remain/binary>> = Rest
  ,{Delta ,Data, Remain}
;
parse_option_delta(Delta ,Len ,Rest) when Delta =:=15 andalso Len =:= 15 ->
  {payload ,Rest}
;
parse_option_delta(_ ,_ ,_) ->
  invalid
.

validate_header(Mtype ,Code, Mid, Tokn) ->
  validate_mtype(Mtype)
  andalso validate_code(Code) 
  andalso validate_mid(Mid)
  andalso validate_tokn(Tokn)
.

validate_mtype(Mtype) when is_atom(Mtype) ->
  get_mtype_value(Mtype) =/= false 
;
validate_mtype(Mtype) when is_integer(Mtype) ->
  get_mtype_desc(Mtype) =/= false 
.

validate_code(Code) when is_atom(Code)->
  get_code_value(Code) =/= false
;
validate_code(Code) when is_integer(Code)->
  get_code_desc(Code) =/= false
.

validate_mid(Mid) ->
  is_binary(Mid) andalso byte_size(Mid) =:= 2
.

validate_tokn(Tokn) ->
  is_binary(Tokn) andalso byte_size(Tokn) =< 8
.

validate_option_type(OptType) ->
  get_option_value(OptType) =/= false
.

get_mtype_value('CON') ->
  0
;
get_mtype_value('NON') ->
  1
;
get_mtype_value('ACK') ->
  2
;
get_mtype_value('RST') ->
  3
;
get_mtype_value(_) ->
  false
.

get_mtype_desc(0) ->
  'CON'
;
get_mtype_desc(1) ->
  'NON'
;
get_mtype_desc(2) ->
  'ACK'
;
get_mtype_desc(3) ->
  'RST'
;
get_mtype_desc(_) ->
  false
.

get_code_desc(0) ->
  'EMPTY'
;
get_code_desc(1) ->
  'GET'
;
get_code_desc(2) ->
  'POST'
;
get_code_desc(3) ->
  'PUT'
;
get_code_desc(4) ->
  'DELETE'
;
get_code_desc(65) ->
  'CREATED'
;
get_code_desc(66) ->
  'DELETED'
;
get_code_desc(67) ->
  'VALID'
;
get_code_desc(68) ->
  'CHANGED'
;
get_code_desc(69) ->
  'CONTENT'
;
get_code_desc(95) ->
  'CONTINUE'
;
get_code_desc(128) ->
  'BAD_REQUEST'
;
get_code_desc(129) ->
  'UNAUTHORIZED'
;
get_code_desc(130) ->
  'BAD_OPTION'
;
get_code_desc(131) ->
  'FORBIDDEN'
;
get_code_desc(132) ->
  'NOT_FOUND'
;
get_code_desc(133) ->
  'METHOD_NOT_ALLOWED'
;
get_code_desc(134) ->
  'NOT_ACCEPTABLE'
;
get_code_desc(136) ->
  'REQUEST_ENTITY_INCOMPLETE'
;
get_code_desc(140) ->
  'PRECONDITION_FAILED'
;
get_code_desc(141) ->
  'REQUEST_ENTITY_TOO_LARGE'
;
get_code_desc(143) ->
  'UNSUPPORTED_CONTENT_FORMAT'
;
get_code_desc(160) ->
  'INTERNAL_SERVER_ERROR'
;
get_code_desc(161) ->
  'NOT_IMPLEMENTED'
;
get_code_desc(162) ->
  'BAD_GATEWAY'
;
get_code_desc(163) ->
  'SERVICE_UNAVAILABLE'
;
get_code_desc(164) ->
  'GATEWAY_TIMEOUT'
;
get_code_desc(165) ->
  'PROXYING_NOT_SUPPORTED'
;
get_code_desc(_) ->
  false
.

get_code_value('EMPTY') ->
  0
;
get_code_value('GET') ->
  1
;
get_code_value('POST') ->
  2
;
get_code_value('PUT') ->
  3
;
get_code_value('DELETE') ->
  4
;
get_code_value('CREATED') ->
  65
;
get_code_value('DELETED') ->
  66
;  
get_code_value('VALID') ->
  67
;
get_code_value('CHANGED') ->
  68
;
get_code_value('CONTENT') ->
  69
;
get_code_value('CONTINUE') ->
  95
;
get_code_value('BAD_REQUEST') ->
  128
;
get_code_value('UNAUTHORIZED') ->
  129
;
get_code_value('BAD_OPTION') ->
  130
;
get_code_value('FORBIDDEN') ->
  131
;
get_code_value('NOT_FOUND') ->
  132
;
get_code_value('METHOD_NOT_ALLOWED') ->
  133
;
get_code_value('NOT_ACCEPTABLE') ->
  134
;
get_code_value('REQUEST_ENTITY_INCOMPLETE') ->
  136
;
get_code_value('PRECONDITION_FAILED') ->
  140
;
get_code_value('REQUEST_ENTITY_TOO_LARGE') ->
  141
;
get_code_value('UNSUPPORTED_CONTENT_FORMAT') ->
  143
;
get_code_value('INTERNAL_SERVER_ERROR') ->
  160
;
get_code_value('NOT_IMPLEMENTED') ->
  161
;
get_code_value('BAD_GATEWAY') ->
  162
;
get_code_value('SERVICE_UNAVAILABLE') ->
  163
;
get_code_value('GATEWAY_TIMEOUT') ->
  164
;
get_code_value('PROXYING_NOT_SUPPORTED') ->
  165
;
get_code_value(_) ->
  false
.

get_option_desc(1) ->
  'IF_MATCH'
;
get_option_desc(3) ->
  'URI_HOST'
;
get_option_desc(4) ->
  'ETAG'
;
get_option_desc(5) ->
  'IF_NONE_MATCH'
;
get_option_desc(6) ->
  'OBSERVE'
;
get_option_desc(7) ->
  'URI_PORT'
;
get_option_desc(8) ->
  'LOCATION_PATH'
;
get_option_desc(11) ->
  'URI_PATH'
;
get_option_desc(12) ->
  'CONTENT_FORMAT'
;
get_option_desc(14) ->
  'MAX_AGE'
;
get_option_desc(15) ->
  'URI_QUERY'
;
get_option_desc(17) ->
  'ACCEPT'
;
get_option_desc(20) ->
  'LOCATION_QUERY'
;
get_option_desc(23) ->
  'BLOCK2'
;
get_option_desc(27) ->
  'BLOCK1'
;
get_option_desc(28) ->
  'SIZE2'
;
get_option_desc(35) ->
  'PROXY_URI'
;
get_option_desc(39) ->
  'PROXY_SCHEME'
;
get_option_desc(60) ->
  'SIZE1'
;
get_option_desc(_) ->
  false
.

get_option_value('IF_MATCH') ->
  1
;
get_option_value('URI_HOST') ->
  3
;
get_option_value('ETAG') ->
  4
;
get_option_value('IF_NONE_MATCH') ->
  5
;
get_option_value('OBSERVE') ->
  6
;
get_option_value('URI_PORT') ->
  7
;
get_option_value('LOCATION_PATH') ->
  8
;
get_option_value('URI_PATH') ->
  11
;
get_option_value('CONTENT_FORMAT') ->
  12
;
get_option_value('MAX_AGE') ->
  14
;
get_option_value('URI_QUERY') ->
  15
;
get_option_value('ACCEPT') ->
  17
;
get_option_value('LOCATION_QUERY') ->
  20
;
get_option_value('BLOCK2') ->
  23
; 
get_option_value('BLOCK1') ->
  27
;
get_option_value('SIZE2') ->
  28
;
get_option_value('PROXY_URI') ->
  35
;
get_option_value('PROXY_SCHEME') ->
  39
;
get_option_value('SIZE1') ->
  60
;
get_option_value(_) ->
  false
.

%get_content_type(0) ->
%  "text/plain"
%;
%get_content_type(40) ->
%  "application/link-format"
%;  
%get_content_type(41) ->
%  "application/xml"
%;
%get_content_type(42) ->
%  "application/octet-stream"
%;
%get_content_type(47) ->
%  "application/exi"
%;
%get_content_type(50) ->
%  "application/json"
%;
%get_content_type(60) ->
%  "application/cbor"
%;
%get_content_type(_) ->
%  false
%.
