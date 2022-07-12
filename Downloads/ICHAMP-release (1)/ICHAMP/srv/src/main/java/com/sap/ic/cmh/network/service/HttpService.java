package com.sap.ic.cmh.network.service;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.Gson;
import com.sap.cds.services.ErrorStatuses;
import com.sap.cds.services.ServiceException;
import com.sap.cds.services.messages.Messages;
import com.sap.cloud.sdk.cloudplatform.connectivity.DefaultHttpClientFactory;
import com.sap.cloud.sdk.cloudplatform.connectivity.DestinationAccessor;
import com.sap.cloud.sdk.cloudplatform.connectivity.HttpDestination;
import com.sap.cloud.sdk.cloudplatform.connectivity.HttpDestinationProperties;
import com.sap.cloud.sdk.cloudplatform.connectivity.RfcDestination;
import com.sap.cloud.sdk.cloudplatform.connectivity.ScpCfDestinationLoader;
import com.sap.ic.cmh.claim.model.ResponseModel;
import com.sap.ic.cmh.claim.model.binary_relation.BinaryRelationDataModel;
import com.sap.ic.cmh.claim.model.binary_relation.BinaryRelationObject;
import com.sap.ic.cmh.claim.model.binary_relation.DocumentFlow;
import com.sap.ic.cmh.gen.MessageKeys;
import com.sap.ic.cmh.model.OAuthToken;
import com.sap.ic.cmh.network.model.ResponseDto;
import com.sap.ic.cmh.objectstore.model.DestinationConfiguration;
import com.sap.ic.cmh.passport.interceptor.SAPPassportInterceptor;
import com.sap.ic.cmh.utils.CommonFunctions;
import com.sap.ic.cmh.utils.Constants;
import com.sap.ic.cmh.utils.LocaleMessageHelper;
import com.sap.ic.cmh.utils.PlatformUtil;

import io.vavr.control.Option;

@Component
public class HttpService {

    @Autowired
    DestinationService destinationService;

    @Autowired
    Messages messages;

    @Autowired
    SAPPassportInterceptor sapPassportInterceptor;

    @Autowired
    CommonFunctions commonFunctions;
    @Autowired
    LocaleMessageHelper localeMessageHelper;

    @Autowired
    PlatformUtil platformUtil;

    Logger log = LoggerFactory.getLogger(HttpService.class);

    private static final String DESTINATION = "destination";
    private static final String URI = "uri";
    private static final String CLIENT_ID = "clientid";
    private static final String CLIENT_SECRET = "clientsecret";
    private static final String UAA_DOMAIN = "uaadomain";
    private static final String BEARER = "Bearer ";

    DefaultHttpClientFactory customFactory = new DefaultHttpClientFactory() {
        @Override
        protected HttpClientBuilder getHttpClientBuilder(HttpDestinationProperties destination) {
            return super.getHttpClientBuilder(destination).addInterceptorFirst(sapPassportInterceptor);
        }
    };

    private CloseableHttpClient getHttpClient() {
        return HttpClients.custom().addInterceptorFirst(sapPassportInterceptor).build();
    }

    private String getBearerString(String token) {
        return BEARER + token;
    }

    public ResponseModel callCpiFlow(String appendedUrl, Map<String, Object> businessObjectMap, String cpiDestination)
            throws IOException {
        String businessObjectNumber = null;
        ResponseModel responseModel;
        ObjectMapper objectMapper = new ObjectMapper();
        String json = executeHttpPost(appendedUrl, businessObjectMap, cpiDestination, objectMapper);
        responseModel = objectMapper.readValue(json, ResponseModel.class);

        businessObjectNumber = responseModel.getResult();
        if (!businessObjectNumber.isEmpty() && !businessObjectNumber.equals("-")) {
            return responseModel;
        } else {
            String messageKey = localeMessageHelper.getMessage(MessageKeys.ERROR_IN_CPI_FLOW);
            String sMessage = messageKey.concat("\n").concat(responseModel.getErrorMessage());
            messages.error(sMessage);
        }
        return null;
    }

    public void callCpiFlowForautoCreation(String appendedUrl, Map<String, Object> businessObjectMap,
            String cpiDestination, String subdomain) throws IOException {
        ObjectMapper objectMapper = new ObjectMapper();
        executeHttpPostforAutoCreation(appendedUrl, businessObjectMap, cpiDestination, subdomain,
                objectMapper);

    }

    private String getJwtToken(Object tokenObject) {
        String jsonString = new Gson().toJson(tokenObject);
        log.info("jsonStringjwt={}", jsonString);
        JSONArray extensionTemplateJSONArray = new JSONArray(jsonString);
        JSONObject jwtObj = extensionTemplateJSONArray.getJSONObject(0);
        log.info("token value={}", jwtObj.getString("value"));
        return jwtObj.getString("value");
    }

    private String getBaseDestinationUrl(Object destinationObject) throws IOException {
        String jsonString = new Gson().toJson(destinationObject);
        ObjectMapper om = new ObjectMapper();
        om.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        DestinationConfiguration destinationConfiguration = om.readValue(jsonString, DestinationConfiguration.class);
        return destinationConfiguration.getuRL();

    }

    public String executeHttpPostforAutoCreation(String appendedUrl, Map<String, Object> businessObjectMap,
            String cpiDestination, String subdomain, ObjectMapper objectMapper) throws IOException {
        String inputJson = "";
        HttpHeaders headers = new HttpHeaders();
        JSONObject destinationDetails = platformUtil.getCredentials(DESTINATION);
        RestTemplate restTemplate = new RestTemplateBuilder().basicAuthentication(
                destinationDetails.getString(CLIENT_ID), destinationDetails.getString(CLIENT_SECRET)).build();
        ResponseEntity<OAuthToken> response1 = restTemplate.getForEntity("https://" + subdomain + "."
                + destinationDetails.getString(UAA_DOMAIN) + "/oauth/token?grant_type=client_credentials",
                OAuthToken.class);

        if (response1.getBody() != null) {
            headers.add("Authorization", BEARER  + response1.getBody().getAccessToken());
        }
        org.springframework.http.HttpEntity<String> entity = new org.springframework.http.HttpEntity<>(headers);
        String destinationDetailsUrl = destinationDetails.getString(URI)
                + "/destination-configuration/v1/destinations/" + Constants.DESTINATION_EXTENSIBILITY;
        ParameterizedTypeReference<Map<String, Object>> typeRef = new ParameterizedTypeReference<Map<String, Object>>() {
        };
        ResponseEntity<Map<String, Object>> destinationResponse = restTemplate.exchange(destinationDetailsUrl,
                HttpMethod.GET, entity, typeRef);
        Object destinationConfigurationMap = Objects.requireNonNull(destinationResponse.getBody())
                .get("destinationConfiguration");
        Object tokenObject = Objects.requireNonNull(destinationResponse.getBody()).get("authTokens");
        String jwt = getJwtToken(tokenObject);
        String baseDestinationUrl = getBaseDestinationUrl(destinationConfigurationMap);

        log.info("created destination");
        HttpClient httpClient = customFactory.createHttpClient();
        HttpPost httpPost = new HttpPost(baseDestinationUrl + appendedUrl);
        inputJson = objectMapper.writeValueAsString(businessObjectMap);
        log.info("Input JSON :::: {}", inputJson);
        StringEntity entityJson = new StringEntity(inputJson);
        httpPost.setEntity(entityJson);
        httpPost.setHeader(Constants.ACCEPT, Constants.APPLICATIONJSON);
        httpPost.setHeader(Constants.CONTENT_TYPE, Constants.APPLICATIONJSON);
        httpPost.addHeader(Constants.DESTINATION, cpiDestination);
        httpPost.setHeader("Authorization", BEARER  + jwt);

        HttpResponse response = httpClient.execute(httpPost);
        org.apache.http.HttpEntity entity1 = response.getEntity();

        String json = EntityUtils.toString(entity1);
        objectMapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);

        return json;

    }

    public String executeHttpPost(String appendedUrl, Map<String, Object> businessObjectMap, String cpiDestination,
            ObjectMapper objectMapper) throws IOException {
        String inputJson = "";
        HttpDestination destination = DestinationAccessor.getDestination(Constants.BASE_DESTINATION).asHttp();
        HttpClient httpClient = customFactory.createHttpClient(destination);
        HttpPost httpPost = new HttpPost(destination.getUri() + appendedUrl);
        inputJson = objectMapper.writeValueAsString(businessObjectMap);
        log.info("Input JSON :::: {}", inputJson);
        StringEntity entityJson = new StringEntity(inputJson);
        httpPost.setEntity(entityJson);
        httpPost.setHeader(Constants.ACCEPT, Constants.APPLICATIONJSON);
        httpPost.setHeader(Constants.CONTENT_TYPE, Constants.APPLICATIONJSON);
        httpPost.addHeader(Constants.DESTINATION, cpiDestination);
        HttpResponse response = httpClient.execute(httpPost);
        HttpEntity entity = response.getEntity();

        String json = EntityUtils.toString(entity);
        objectMapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);
        return json;
    }

    public void createDocumentFlowObjects(ScpCfDestinationLoader scpCfDestinationLoader, String objectANumber,
            String objectAType, String objectBNumber, String objectBType, String cpiDestination,
            String targetDestination) {
        try {
            String rfcDestination = getLogicalSystem(scpCfDestinationLoader, cpiDestination);
            String sourceLogicalSystem = getTargetLogicalSystem(scpCfDestinationLoader, rfcDestination);
            String targetLogicalSystem = getLogicalSystem(scpCfDestinationLoader, targetDestination);
            DocumentFlow setDocumentFlowRequest = commonFunctions.setDocumentFlowRequest(objectANumber, objectAType,
                    objectBNumber, objectBType, sourceLogicalSystem, targetLogicalSystem);
            setBinaryRelationObjects(setDocumentFlowRequest);
            createDocumentFlow(setDocumentFlowRequest.getObjectA(), setDocumentFlowRequest.getObjectB(),
            rfcDestination);
        } catch (IOException e) {
            throw new ServiceException(ErrorStatuses.BAD_REQUEST, MessageKeys.ERROR_BO_CREATION);
        }
    }

    public void setBinaryRelationObjects(DocumentFlow setDocumentFlowRequest) {
        BinaryRelationObject objectA = setDocumentFlowRequest.getObjectA();
        objectA.setObjectKey(setDocumentFlowRequest.getObjectANumber());
        log.info("objectANumber  is :::: {}", setDocumentFlowRequest.getObjectANumber());
        objectA.setObjectType(setDocumentFlowRequest.getObjectAType());
        log.info("objectAType  is :::: {}", setDocumentFlowRequest.getObjectAType());
        objectA.setLogSys(setDocumentFlowRequest.getSourceLogicalSystem());
        BinaryRelationObject objectB = setDocumentFlowRequest.getObjectB();
        objectB.setObjectKey(setDocumentFlowRequest.getObjectBNumber());
        log.info("objectBNumber  is :::: {}", setDocumentFlowRequest.getObjectBNumber());
        objectB.setObjectType(setDocumentFlowRequest.getObjectBType());
        log.info("objectBType  is :::: {}", setDocumentFlowRequest.getObjectBType());
        objectB.setLogSys(setDocumentFlowRequest.getTargetLogicalSystem());
        log.info("logicalSystem  is :::: {}", setDocumentFlowRequest.getTargetLogicalSystem());
    }

    public String getLogicalSystem(ScpCfDestinationLoader scpCfDestinationLoader, String bapiDestination) {
        try {
            log.info("Logical system for doc flow is  {} ", bapiDestination);
            HttpDestination destination = destinationService.getHttpDestination(scpCfDestinationLoader,
                    bapiDestination);
            Option<Object> option = destination.get("description");
            return option.get().toString();
        } catch (NoSuchElementException e) {
            throw new ServiceException(ErrorStatuses.BAD_REQUEST, MessageKeys.DESCRIPTION_IN_DESTINATION_NOT_EXIST);
        }
    }

    public String getTargetLogicalSystem(ScpCfDestinationLoader scpCfDestinationLoader, String targetDestination) {
        try {
            log.info("Logical system for doc flow is  {} ", targetDestination);
            RfcDestination destination = destinationService.getRfcDestination(scpCfDestinationLoader,
                    targetDestination);
            Option<Object> option = destination.get("jco.destination.description");
            return option.get().toString();
        } catch (NoSuchElementException e) {
            throw new ServiceException(ErrorStatuses.BAD_REQUEST, MessageKeys.DESCRIPTION_IN_DESTINATION_NOT_EXIST);
        }
    }

    public void createDocumentFlow(BinaryRelationObject objectA, BinaryRelationObject objectB, String cpiDestination)
            throws IOException {
        ObjectMapper oMapper = new ObjectMapper();
        Map<String, Object> binaryRelationDataModelMap = setRequestForDocumentFlow(objectA, objectB, oMapper);
        String json = executeHttpPost(Constants.DOCUMENT_FLOW_APPENDED_URL, binaryRelationDataModelMap, cpiDestination,
                oMapper);
        Map<String, Object> responseMap = oMapper.readValue(json, Map.class);

        if (responseMap.containsKey("error")) {
            throw new ServiceException(ErrorStatuses.BAD_REQUEST, MessageKeys.ERROR_BO_CREATION);
        } else {
            log.info("Document Flow Successfully Created");
        }
    }

    Map<String, Object> setRequestForDocumentFlow(BinaryRelationObject objectA, BinaryRelationObject objectB,
            ObjectMapper oMapper) {
        BinaryRelationDataModel binaryRelationDataModel = new BinaryRelationDataModel();
        binaryRelationDataModel.setObjectA(objectA);
        binaryRelationDataModel.setObjectB(objectB);
        binaryRelationDataModel.setRelationType(Constants.BINARY_RELATIONSHIP_TYPE);
        return oMapper.convertValue(binaryRelationDataModel, Map.class);
    }

    public ResponseDto get(String url, String token, Map<String, String> headers) throws IOException {
        try (CloseableHttpClient client = getHttpClient()) {
            HttpGet httpGet = new HttpGet(url);
            httpGet.addHeader(HttpHeaders.AUTHORIZATION, getBearerString(token));
            headers.entrySet().stream().forEach(header -> httpGet.addHeader(header.getKey(), header.getValue()));
            try (CloseableHttpResponse response = client.execute(httpGet)) {
                return ResponseDto.builder().status(response.getStatusLine().getStatusCode())
                        .content(EntityUtils.toString(response.getEntity(), StandardCharsets.UTF_8)).build();
            }
        }
    }
}
