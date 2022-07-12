package com.sap.ic.cmh.utils;
public final class Constants {

    public static final String SAAS_REGISTRY_SERVICE_LABEL_NAME = "saas-registry";
	public static final String URL = "url";
	public static final String CLIENT_ID = "clientid";
	public static final String CLIENT_SECRET = "clientsecret";
	public static final String SAAS_REGISTRY_URL = "saas_registry_url";
	public static final String SAAS_MANAGER = "/saas-manager/v1/application/subscriptions?";

	/**
	 * 
	 */
	private Constants() {
		// Default constructor
	}

	/*--------------------------------------------- Start of General Objects ---------------------------------------------------*/
	public static final String ID = "ID";
	public static final String FAILURE = "Failure";
	public static final String SUCCESS = "Success";
	/*--------------------------------------------- End of General Objects ---------------------------------------------------*/
	
    /*--------------------------------------------- Start of Field Control ---------------------------------------------------*/

	public static final int FIELD_CONTROL_READ_ONLY = 1;
	public static final int FIELD_CONTROL_OPTIONAL = 3;
	public static final int FIELD_CONTROL_MANDATORY = 7;
	/*--------------------------------------------- End of Field Control -----------------------------------------------------*/
	
    /*--------------------------------------------- Start of Complaint Objects -----------------------------------------------------*/
	public static final String COMPLAINT_ID = "complaint_ID";
	public static final String COMPLAINT_IN_PROGRESS = "INPR";
	public static final String COMPLAINT_CLOSED = "CLSD";
	public static final String COMPLAINT_DISCARDED = "DISCD";
	public static final String COMPLAINT_REVISED = "REVSD";
	public static final String COMPLAINT_TYPE = "SREC";
	public static final String COMPLAINT_MANUAL = "Manual";
	public static final String COMPLAINT_AUTOMATIC = "Automatic";
	public static final String COMPLAINT_AUTOMATIC_USER_NAME = "System";
	public static final String COMPLAINT_CREATED_STATUS = "CRTD";
	public static final String DEFAULT_LABOR_UNIT = "HR";
    
	public static final Integer MAX_LENGTH = 21;
	/*--------------------------------------------- End of Complaint Objects -----------------------------------------------------*/
	
    /*--------------------------------------------- Start of Streams -----------------------------------------------------*/
	public static final String QUALITY_STREAM_CODE = "QLTY";
	/*--------------------------------------------- End of Streams -----------------------------------------------------*/
	
    /*--------------------------------------------- Start of Business Objects -----------------------------------------------------*/
    public static final String STATUS_NEW = "NEW";
    public static final String OBJECT_NUMBER = "objectNumber";
	/*--------------------------------------------- End of Business Objects -----------------------------------------------------*/

    /*--------------------------------------------- Start of Quality Notification -----------------------------------------------------*/
	public static final String QUALITYNOTIFICATION_CODE = "QN";
	public static final String QN_STATUS_CREATED = "QNCRTD";
	public static final String QN_STATUS_CLOSED = "QNCMPL";
	public static final String DEFECT_IDENTIFIER = "0001";
	public static final String QN_CREATE_BACKEND_STATUS = "I0068";
    /*--------------------------------------------- End of Quality Notification -----------------------------------------------------*/
    
    /*--------------------------------------------- Start of Claim -----------------------------------------------------*/
	public static final String CLAIM_CODE = "CLM";
	public static final String CLAIM_CREATED = "CLMCRTD";
	public static final String CLAIM_STATUS_CLOSED = "CLMCLSD";
	/*--------------------------------------------- End of Claim -----------------------------------------------------*/
   
    /*--------------------------------------------- Start of Return Purchase Order -----------------------------------------------------*/
    public static final String RETURNPO_CODE = "RPO";
    public static final String RPO_STATUS_CREATED = "RPOCRTD";
	public static final String RPO_STATUS_CLOSED = "RPOCLSD";
	public static final String RPO_BACKEND_STATUS_CREATED = "RPO";
    /*--------------------------------------------- End of Return Purchase Order -----------------------------------------------------*/
    
    /*--------------------------------------------- Start of Supplier Eight D -----------------------------------------------------*/
	public static final String SUPPLIER_EIGHTD_CODE = "S8D";
    public static final String SUPPLIER_ISSUE_PROCESS_STATUS_CREATED = "S8DCRTD";
    public static final String SUPPLIER_ISSUE_PROCESS_STATUS_CLOSED = "S8DCMPL";
	public static final String EIGHTD_CREATE_BACKEND_STATUS = "10";
	public static final String LIFECYCLE_STATUS_CODE="LC";
	public static final String CONFIRMATION_STATUS_CODE="CONF";
    public static final String IS_EIGHTD_RELEVANT = "isRelevant";
	/*--------------------------------------------- End of Supplier Eight D -----------------------------------------------------*/

	/*---------------------------------------------Start OF HttpService constants------------------------------------------*/
	public static final String ACCEPT = "Accept";
	public static final String CONTENT_TYPE = "Content-type";
	public static final String APPLICATIONJSON = "application/json";
	public static final String DOCUMENT_FLOW_APPENDED_URL = "/http/createBinaryRelation";
	public static final String QN_APPENDED_URL = "/http/createQualityNotification";
	public static final String RETURN_PO_APPENDED_URL = "/http/createreturnpurchaseorder";
	public static final String CLAIM_APPENDED_URL = "/http/createClaim";
	public static final String EXCHANGE_RATE_APPENDED_URL = "/http/getExchangeRate";
	public static final String GET_RPO_APPENDED_URL = "/http/getreturnpo";
    public static final String CREATE_UPDATE_QN = "/http/qnautocreate";
    public static final String UPDATE_CLAIM_STATUS = "/http/updateclaimstatus";
    public static final String UPDATE_RPO_STATUS = "/http/updatereturnpurchaseorder";
	public static final String DESTINATION = "destination";
    public static final String DESTINATION_EXTENSIBILITY = "ExtendComplaintHandling";
	/*---------------------------------------------End OF HttpService constants------------------------------------------*/

	/*---------------------------------------------Start of BAPI constants------------------------------------------*/
	public static final String BINARY_RELATIONSHIP_TYPE = "VONA";
	public static final String CLAIM_OBJECT_TYPE = "BUS2222";
	public static final String QUALITY_NOTIFICATION_OBJECT_TYPE = "BUS2078";
	public static final String EIGHTD_OBJECT_TYPE = "QPROBLEMSP";
	public static final String RETURN_PURCHASE_ORDER_OBJECT_TYPE = "BUS2012";
	/*---------------------------------------------End of BAPI constants------------------------------------------*/

	/*-----------------------------------------Start of Audit Log Constants--------------------------------*/
	public static final String ENTITY_NAME = "entityName";
    public static final String OBJECT_NAME = "objectName";
    public static final String UPSERT_OP = "Upsert";
    public static final String DELETE_OP = "Delete";
    public static final String AUDIT_USER_NAME = "name";
    public static final String AUDIT_USER_TYPE = "user";
    public static final String OBJECT_ID = "objectId";
    public static final String AUDIT_OP = "operation";
    public static final String AUDIT_ID_PROVIDER = "Customer";

	/*-----------------------------------------End of Audit Log Constants--------------------------------*/

	/*-----------------------------------------Start of Destination Constants--------------------------------*/
	public static final String BASE_DESTINATION = "CPIintegration";
	/*-----------------------------------------End of Destination Constants--------------------------------*/

	/*---------------------------------------------- START of Formats --------------------------------------------------------*/
	public static final String EMAIL = "^[\\w-\\+]+(\\.[\\w]+)*@[\\w-]+(\\.[\\w]+)*(\\.[a-z]{2,})$";
	public static final String MOBILE = "\\+?\\d[\\d -]{8,12}\\d";
	public static final String ALPHABETS = "^[a-zA-Z ]*$";
	public static final String ALPHANUMERIC = "^[A-Za-z0-9 ]*$";
	public static final String NUMBERS = "^[0-9]*$";
    public static final String DEFCODERESTRCHAR = "^[^+%@_*]*$";
    public static final String WTVOLUNITACCEPCHAR = "^[A-Za-z0-9%]*$";
    public static final String BASEUOMACCEPCHAR = "^[A-Za-z0-9%/ ]*$";
	/*---------------------------------------------- END of Formats --------------------------------------------------------*/

	/*--------------------------------------------- START of Exchange Partner Type -------------------------------------------*/
    public static final String EXCHANGE_PARTNER_TYPE_SUPPLER = "SUP";
    public static final String EXCHANGE_PARTNER_TYPE_SUPPLER_CONTACT = "SUPCON";   
    public static final String EXCHANGE_PARTNER_TYPE_PERSON_RESPONSIBLE = "PERRES";
    /*--------------------------------------------- END of Exchange Partner Type ---------------------------------------------*/

    /*--------------------------------------------- START of Currency Conversion Parameters -------------------------------------------*/
    public static final String RATE_TYPE = "RATE_TYPE";
    public static final String FROM_CURRENCY = "FROM_CURR";
    public static final String TO_CURRENCY = "TO_CURRNCY";
    public static final String STRING = "String";
    public static final String REF_DATE = "DATE";
    /*--------------------------------------------- END of Currency Conversion Parameters ---------------------------------------------*/

    /*--------------------------------------------- START of Job Scheduler Controller Constants -------------------------------------------*/
    public static final String JOB_SCHEDULER_SUPPLIER_ISSUE_PROCESS_API = "sipstatus";
    public static final String JOB_SCHEDULER_RPO_API = "rpostatus";
    /*--------------------------------------------- END of Job Scheduler Controller Constants ---------------------------------------------*/

    /*--------------------------------------------- START of Return Purchase Order RFC Constants -------------------------------------------*/
    public static final String RPO_RFC_NAME = "Z_DOC_STAT";
    public static final String RPO_RFC_INPUT = "IT_RPO";
    public static final String RPO_RFC_INPUT_TYPE = "ZTTRPO";
    public static final String RPO_RFC_INPUT_NUMBER = "RPO";
    public static final String RPO_RFC_INPUT_TYPE_STRING = "ZTTRPO";
    public static final String RPO_RFC_OUTPUT = "ET_DOC";
    public static final String RPO_RFC_OUTUT_TYPE = "ZTTDOC";
    /*--------------------------------------------- END of Return Purchase Order RFC Constants ---------------------------------------------*/

	/*--------------------------------------------- START of Cloud Foundry Constants -------------------------------------------*/
	public static final String REGION = "region";
	public static final String UAA_CLIENT_ID = "clientid";
	public static final String UAA_CLIENT_SECRET = "clientsecret";
	public static final String TENANT_ID = "tenantid";
	public static final String TOKEN_URL = "token_url";
	/*--------------------------------------------- END of Cloud Foundry Constants -------------------------------------------*/

	/*--------------------------------------------- START of Metering Constants -------------------------------------------*/
	public static final String METERING_SERVICE = "metering-service";
	public static final String METERING_URL = "metering_url";
	public static final String METERING_TIMESTAMP = "yyyy-MM-dd'T'HH:mm:ss.SSS";
	public static final String CONSUMER_ENVIRONMENT = "CF";
	public static final String MEASURE_ID = "records";
	public static final String SERVICE_ID = "cmh-service";
	public static final String SERVICE_PLAN = "api-access";
	/*--------------------------------------------- END of Metering Constants -------------------------------------------*/

	/*--------------------------------------------- START of Passport Constants -------------------------------------------*/
	public static final String SAP_PASSPORT = "sap-passport";
	public static final String DSR_CONNECTION_KEY = "DSRConnection";
	public static final String DSR_COUNTER_KEY = "DSRCounter";
	public static final String DSR_TRANSACTION_KEY = "DSRTransaction";
	public static final String DSR_ROOT_CONTEXT_ID_KEY = "DSRRootContextId";
	public static final String DSR_PREV_COMP = "DSRPrevComp";
	/*--------------------------------------------- END of Passport Constants -------------------------------------------*/
    
    /*--------------------------------------------- START of Configuration -------------------------------------------*/
    public static final String ITEM_TYPE_FR = "FR";
    /*--------------------------------------------- END of Configuration -------------------------------------------*/
    /*--------------------------------------------- START of Complaint Type -------------------------------------------*/
    public static final String CUSTOMER_COMPLAINT_TYPE = "CUSCMP";
    /*--------------------------------------------- START of Complaint Type -------------------------------------------*/
    /*--------------------------------------------- START of Attachments -------------------------------------------*/
    public static final String MIME_TYPE_JPEG= "image/jpeg";
    public static final String MIME_TYPE_PNG="image/png";
    public static final String MIME_TYPE_GIF="image/gif";
    public static final String MIME_TYPE_DOCX="application/vnd.openxmlformats-officedocument.wordprocessingml.document";
    public static final String MIME_TYPE_DOC="application/msword";
    public static final String MIME_TYPE_XLS="application/vnd.ms-excel";
    public static final String MIME_TYPE_XLSX="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
    public static final String MIME_TYPE_PDF="application/pdf";
    /*--------------------------------------------- START of Attachments -------------------------------------------*/  
}
