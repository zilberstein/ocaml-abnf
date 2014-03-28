This isnx a program

type LWS = 
  | Rule of (rule, rule)
type tspecials = 
r
type token = 
r
type comment = 
  | Rule of (rule, rule)
type ctext = 
r
type quoted_string = 
r
type qdtext = 
r
type quoted_pair = 
  | Rule of (rule, rule)
type HTTP_Version = 
  | Rule of (rule, rule)
type URI = 
  | Rule of (rule, rule)
type absoluteURI = 
  | Rule of (rule, rule)
type relativeURI = 
r
type net_path = 
  | Rule of (rule, rule)
type abs_path = 
  | Rule of (rule, rule)
type rel_path = 
  | Rule of (rule, rule)
type path = 
  | Rule of (rule, rule)
type fsegment = 
r
type segment = 
r
type params = 
  | Rule of (rule, rule)
type param = 
r
type scheme = 
r
type net_loc = 
r
type query = 
r
type fragment = 
r
type pchar = 
r
type uchar = 
r
type unreserved = 
r
type escape = 
  | Rule of (rule, rule)
type reserved = 
r
type extra = 
r
type safe = 
r
type unsafe = 
r
type national = 
r
type http_URL = 
  | Rule of (rule, rule)
type host = 
r
type port = 
r
type HTTP_date = 
r
type rfc1123_date = 
  | Rule of (rule, rule)
type rfc850_date = 
  | Rule of (rule, rule)
type asctime_date = 
  | Rule of (rule, rule)
type date1 = 
  | Rule of (rule, rule)
type date2 = 
  | Rule of (rule, rule)
type date3 = 
  | Rule of (rule, rule)
type time = 
  | Rule of (rule, rule)
type wkday = 
r
type weekday = 
r
type month = 
r
type delta_seconds = 
r
type charset = 
token
type content_coding = 
token
type transfer_coding = 
r
type transfer_extension = 
token
type Chunked_Body = 
  | Rule of (rule, rule)
type chunk = 
  | Rule of (rule, rule)
type hex_no_zero = 
r
type chunk_size = 
  | Rule of (rule, rule)
type chunk_ext = 
r
type chunk_ext_name = 
token
type chunk_ext_value = 
r
type chunk_data = 
  | Rule of (rule, rule)
type footer = 
r
type media_type = 
  | Rule of (rule, rule)
type type = 
token
type subtype = 
token
type parameter = 
  | Rule of (rule, rule)
type attribute = 
token
type value = 
r
type product = 
  | Rule of (rule, rule)
type product_version = 
token
type qvalue = 
r
type language_tag = 
  | Rule of (rule, rule)
type primary_tag = 
r
type subtag = 
r
type entity_tag = 
  | Rule of (rule, rule)
type weak = 
W/
type opaque_tag = 
quoted-string
type range_unit = 
r
type bytes_unit = 
bytes
type other_range_unit = 
token
type HTTP_message = 
r
type generic_message = 
  | Rule of (rule, rule)
type start_line = 
r
type message_header = 
  | Rule of (rule, rule)
type field_name = 
token
type field_value = 
r
type field_content = 
r
type message_body = 
entity-body
type general_header = 
r
type Request = 
r
type Request_Line = 
  | Rule of (rule, rule)
type Method = 
r
type extension_method = 
token
type Request_URI = 
r
type request_header = 
r
type Response = 
  | Rule of (rule, rule)
type Status_Line = 
  | Rule of (rule, rule)
type Status_Code = 
r
type Reason_Phrase = 
r
type response_header = 
r
type entity_header = 
r
type extension_header = 
message-header
type entity_body = 
r
type auth_scheme = 
token
type auth_param = 
  | Rule of (rule, rule)
type challenge = 
  | Rule of (rule, rule)
type realm = 
  | Rule of (rule, rule)
type realm_value = 
quoted-string
type credentials = 
r
type basic_credentials = 
  | Rule of (rule, rule)
type base64 = 
  | Rule of (rule, rule)
type base64_char = 
r
type base64_terminal = 
r
type basic_cookie = 
base64
type user_pass = 
  | Rule of (rule, rule)
type userid = 
r
type password = 
r
type Accept = 
  | Rule of (rule, rule)
type media_range = 
  | Rule of (rule, rule)
type accept_params = 
  | Rule of (rule, rule)
type accept_extension = 
  | Rule of (rule, rule)
type Accept_Charset = 
  | Rule of (rule, rule)
type Accept_Encoding = 
  | Rule of (rule, rule)
type Accept_Language = 
  | Rule of (rule, rule)
type language_range = 
r
type Accept_Ranges = 
  | Rule of (rule, rule)
type acceptable_ranges = 
r
type Age = 
  | Rule of (rule, rule)
type age_value = 
delta-seconds
type Allow = 
  | Rule of (rule, rule)
type Authorization = 
  | Rule of (rule, rule)
type Cache_Control = 
  | Rule of (rule, rule)
type cache_directive = 
r
type cache_request_directive = 
r
type cache_response_directive = 
r
type cache_extension = 
  | Rule of (rule, rule)
type Connection = 
Connection-header
type Connection_header = 
  | Rule of (rule, rule)
type connection_token = 
token
type Content_Base = 
  | Rule of (rule, rule)
type Content_Encoding = 
  | Rule of (rule, rule)
type Content_Language = 
  | Rule of (rule, rule)
type Content_Length = 
  | Rule of (rule, rule)
type Content_Location = 
  | Rule of (rule, rule)
type Content_MD5 = 
  | Rule of (rule, rule)
type md5_digest = 
base64
type Content_Range = 
  | Rule of (rule, rule)
type content_range_spec = 
byte-content-range-spec
type byte_content_range_spec = 
  | Rule of (rule, rule)
type entity_length = 
r
type Content_Type = 
  | Rule of (rule, rule)
type Date = 
  | Rule of (rule, rule)
type ETag = 
  | Rule of (rule, rule)
type Expires = 
  | Rule of (rule, rule)
type From = 
  | Rule of (rule, rule)
type mailbox = 
TEXT
type Host = 
  | Rule of (rule, rule)
type If_Modified_Since = 
  | Rule of (rule, rule)
type If_Match = 
  | Rule of (rule, rule)
type If_None_Match = 
  | Rule of (rule, rule)
type If_Range = 
  | Rule of (rule, rule)
type If_Unmodified_Since = 
  | Rule of (rule, rule)
type Last_Modified = 
  | Rule of (rule, rule)
type Location = 
  | Rule of (rule, rule)
type Max_Forwards = 
  | Rule of (rule, rule)
type Pragma = 
  | Rule of (rule, rule)
type pragma_directive = 
r
type extension_pragma = 
  | Rule of (rule, rule)
type Proxy_Authenticate = 
  | Rule of (rule, rule)
type Proxy_Authorization = 
  | Rule of (rule, rule)
type Public = 
  | Rule of (rule, rule)
type ranges_specifier = 
byte-ranges-specifier
type byte_ranges_specifier = 
  | Rule of (rule, rule)
type byte_range_set = 
r
type byte_range_spec = 
  | Rule of (rule, rule)
type first_byte_pos = 
r
type last_byte_pos = 
r
type suffix_byte_range_spec = 
  | Rule of (rule, rule)
type suffix_length = 
r
type Range = 
  | Rule of (rule, rule)
type Referer = 
  | Rule of (rule, rule)
type Retry_After = 
  | Rule of (rule, rule)
type Server = 
  | Rule of (rule, rule)
type Transfer_Encoding = 
  | Rule of (rule, rule)
type Upgrade = 
  | Rule of (rule, rule)
type User_Agent = 
  | Rule of (rule, rule)
type Vary = 
  | Rule of (rule, rule)
type Via = 
  | Rule of (rule, rule)
type received_protocol = 
  | Rule of (rule, rule)
type protocol_name = 
token
type protocol_version = 
token
type received_by = 
r
type pseudonym = 
token
type Warning = 
  | Rule of (rule, rule)
type warning_value = 
  | Rule of (rule, rule)
type warn_code = 
r
type warn_agent = 
r
type warn_text = 
quoted-string
type WWW_Authenticate = 
  | Rule of (rule, rule)
type MIME_Version = 
  | Rule of (rule, rule)
type Content_Version = 
  | Rule of (rule, rule)
type Derived_From = 
  | Rule of (rule, rule)
type Link = 
  | Rule of (rule, rule)
type link_param = 
r
type link_extension = 
  | Rule of (rule, rule)
type relationship = 
r
type sgml_name = 
  | Rule of (rule, rule)
type URI_header = 
  | Rule of (rule, rule)
type Keep_Alive_header = 
  | Rule of (rule, rule)
type keepalive_param = 
  | Rule of (rule, rule)
type param_name = 
r
type TEXT = 
r