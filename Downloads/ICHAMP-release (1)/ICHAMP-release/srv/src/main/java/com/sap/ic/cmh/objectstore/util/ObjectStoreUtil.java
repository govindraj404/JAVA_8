package com.sap.ic.cmh.objectstore.util;

import java.text.DecimalFormat;
import java.util.Map;
import java.util.Objects;

import org.jclouds.blobstore.domain.Blob;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.Gson;
import com.sap.cds.services.runtime.CdsRuntime;
import com.sap.ic.cmh.model.OAuthToken;
import com.sap.ic.cmh.network.service.DestinationService;
import com.sap.ic.cmh.objectstore.model.BlobContent;
import com.sap.ic.cmh.objectstore.model.DestinationConfiguration;
import com.sap.ic.cmh.utils.PlatformUtil;

@Component
public class ObjectStoreUtil {

    @Autowired
    PlatformUtil platformUtil;
    @Autowired
    DestinationService destinationService;
    @Autowired
    CdsRuntime cdsRuntime;

    private static Logger logger = LoggerFactory.getLogger(ObjectStoreUtil.class);
    private static final String DESTINATION = "destination";
    private static final String URI = "uri";
    private static final String CLIENT_ID = "clientid";
    private static final String CLIENT_SECRET = "clientsecret";
    private static final String UAA_DOMAIN = "uaadomain";

    private static final String BYTE = "B";
    
	private static final String KILLOBYTE = "KB";
	private static final String MEGABYTE = "MB";
	private static final String GIGABYTE = "GB";
	private static final String TERABYTE = "TB";
	private static final String DECIMAL_FORMAT_PATTERN = "#,##0.#";

    public String getBucket(String subdomain) {
        logger.info("Getting the bucket details");
        DestinationConfiguration destinationConfiguration = getDestinationConfiguration(subdomain);
        return destinationConfiguration.getBucket();
    }

    public DestinationConfiguration getDestinationConfiguration(String subdomain) {
        logger.info("Getting the destination config details");
        DestinationConfiguration destinationConfiguration = null;
        try {
            JSONObject destinationDetails = platformUtil.getCredentials(DESTINATION);
            RestTemplate restTemplate = new RestTemplateBuilder().basicAuthentication(destinationDetails.getString(CLIENT_ID), destinationDetails.getString(CLIENT_SECRET)).build();
            ResponseEntity<OAuthToken> response = restTemplate.getForEntity( "https://"+ subdomain + "." + destinationDetails.getString(UAA_DOMAIN) + "/oauth/token?grant_type=client_credentials", OAuthToken.class);
            HttpHeaders headers = new HttpHeaders();
            if (response.getBody() != null) {
                headers.add("Authorization", "Bearer " + response.getBody().getAccessToken());
            }
            HttpEntity<String> entity = new HttpEntity<>(headers);
            String destinationDetailsUrl = destinationDetails.getString(URI) + "/destination-configuration/v1/destinations/DataExport";
            ParameterizedTypeReference<Map<String, Object>> typeRef = new ParameterizedTypeReference<Map<String, Object>>() {};
            ResponseEntity<Map<String, Object>> destinationResponse = restTemplate.exchange(destinationDetailsUrl, HttpMethod.GET, entity, typeRef);
            Object destinationConfigurationMap = Objects.requireNonNull(destinationResponse.getBody()).get("destinationConfiguration");
            String jsonString = new Gson().toJson(destinationConfigurationMap);
            ObjectMapper om = new ObjectMapper();
            destinationConfiguration = om.readValue(jsonString, DestinationConfiguration.class);
        } catch (RestClientException e) {
            logger.info("Rest Client Exception");
        } catch (JSONException e) {
            logger.info("JSON Exception");
        } catch (JsonMappingException e) {
            logger.info("JSON Mapping Exception");
        } catch (JsonProcessingException e) {
            logger.info("JSON Processing Exception");
        }
        return destinationConfiguration;
    }

    public BlobContent createBlobFile(final Blob blob) {
		return new BlobContent(
				blob.getMetadata().getETag(), 
				blob.getMetadata().getContainer(), 
				blob.getMetadata().getName(), 
				blob.getMetadata().getUri().toString(),
				readableFileSize(blob.getMetadata().getSize()),
				blob.getMetadata().getLastModified().toString(),
				blob.getPayload().getContentMetadata().getContentType(),
				blob.getMetadata().getUserMetadata()
				);
	}

    public String readableFileSize(long size) {
		if (size <= 0)
			return "0";
		final String[] units = new String[] { BYTE, KILLOBYTE, MEGABYTE, GIGABYTE, TERABYTE };
		int digitGroups = (int) (Math.log10(size) / Math.log10(1024));
		return new DecimalFormat(DECIMAL_FORMAT_PATTERN).format(size / Math.pow(1024, digitGroups)) + " "
				+ units[digitGroups];
	}
}