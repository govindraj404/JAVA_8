package com.sap.ic.cmh.offboarding.service;

import cds.gen.com.sap.ic.cmh.customerdataexportstatus.CustomerDataExportStatuses;
import com.opencsv.CSVWriter;
import com.sap.cds.services.request.RequestContext;
import com.sap.cds.services.runtime.CdsRuntime;
import com.sap.ic.cmh.offboarding.persistency.CustomerDataExportStatusDao;
import com.sap.ic.cmh.model.OAuthToken;
import com.sap.ic.cmh.objectstore.model.UploadStatus;
import com.sap.ic.cmh.objectstore.service.ObjectStoreService;
import com.sap.ic.cmh.utils.LoggerHelper;
import com.sap.ic.cmh.utils.PlatformUtil;
import org.apache.commons.io.FileUtils;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import java.io.*;
import java.sql.*;
import java.time.Instant;
import java.util.Objects;
import java.util.Properties;
import java.util.function.Consumer;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

@Component
public class ExportCustomerDataService {

    @Autowired
    PlatformUtil platformUtil;
    @Autowired
    CustomerDataExportStatusDao customerDataExportStatusDao;
    @Autowired
    CdsRuntime cdsRuntime;
    @Autowired
    ObjectStoreService objectStoreService;

    private static final String SERVICE_MANAGER = "service-manager";
    private static final String AUTH_URL = "url";
    private static final String CLIENT_ID = "clientid";
    private static final String CLIENT_SECRET = "clientsecret";
    private static final String SERVICE_MANAGER_URL = "sm_url";

    private static final Logger logger = LoggerFactory.getLogger(ExportCustomerDataService.class);

    @Async
    public void downloadTenantData(String subdomain, JSONArray tenantDBContainerArray, String tenantId,
            CustomerDataExportStatuses customerDataExportStatuses) {
        logger.info("Number of DB Containers :: {}", tenantDBContainerArray.length());
        String fileName = tenantId + Instant.now().toString() + ".zip";
        logger.info("File Name :: {}", fileName);

        FileOutputStream fos = null;
        ZipOutputStream zos = null;
        InputStream resourceStream = null ;
        try {
            fos = new FileOutputStream(fileName);
            zos = new ZipOutputStream(fos);

            String resourceName = "customerTables.properties";
            Properties props = new Properties();
            ClassLoader loader = Thread.currentThread().getContextClassLoader();
             resourceStream = loader.getResourceAsStream(resourceName);
            props.load(resourceStream);

            for (int i = 0; i < tenantDBContainerArray.length(); i++) {
                JSONObject tenantDBContainer = tenantDBContainerArray.getJSONObject(i);
                JSONObject tenantDBContainerCredentials = tenantDBContainer.getJSONObject("credentials");

                ZipOutputStream finalZos = zos;
                props.keySet().forEach(tableName -> {
                	Statement stmt = null;
                	ResultSet resultSet = null;
                	//moved to a separate method due to code complexity
                    selectDataFromTableAndCreateCsv(tenantDBContainerArray, tenantDBContainer,
							tenantDBContainerCredentials, finalZos, tableName, stmt, resultSet);
                });
            }
            zos.close();
            UploadStatus uploadStatus = objectStoreService.uploadFile(subdomain, FileUtils.readFileToByteArray(new File(fileName)),
                    fileName, "application/zip");
            if (Objects.equals(uploadStatus.getStatus(), "SUCCESS")) {
                customerDataExportStatuses.setStatus("Completed");
                customerDataExportStatuses.setFileName(fileName);
            } else {
                customerDataExportStatuses.setStatus("Failed");
                customerDataExportStatuses.setError(uploadStatus.getErrorMessage());
            }
        } catch (IOException | IllegalArgumentException ioe) {
            customerDataExportStatuses.setStatus("Failed");
            customerDataExportStatuses.setError(ioe.getMessage());
        } finally {
            try {
                if (zos != null) {
                    zos.close();
                }
                if (fos != null) {
                    fos.close();
                }
                //FIX Fortify issue
                if(resourceStream!=null) {
					resourceStream.close();
				}
            } catch (IOException ioe) {
                logger.error("Error in closing the Stream");
            }
        }

        customerDataExportStatuses.setModifiedAt(Instant.now());
        cdsRuntime.requestContext().privilegedUser().modifyUser(user -> user.setTenant(tenantId))
                .run((Consumer<RequestContext>) context -> customerDataExportStatusDao
                        .updateCustomerDataExportStatus(customerDataExportStatuses));
        logger.info("Tenant data export completed");
    }

	public void selectDataFromTableAndCreateCsv(JSONArray tenantDBContainerArray, JSONObject tenantDBContainer,
			JSONObject tenantDBContainerCredentials, ZipOutputStream finalZos, Object tableName, Statement stmt,
			ResultSet resultSet) {
		try {
		    Connection connection = getHandler(tenantDBContainer);
		    stmt = connection.createStatement();
		    String table = tenantDBContainerCredentials.getString("schema") + "." + tableName;
		    String query = "Select * from " + table;
		    resultSet = stmt.executeQuery(query);

		    createCsvAndZip(tenantDBContainerArray, tenantDBContainerCredentials, tableName, finalZos,
		            resultSet);

		    connection.close();
		} catch (SQLException ioe) {
			//FIX Fortify issue
			LoggerHelper.logExceptionWithMessage(logger, "Error while writing the data ", ioe);
		    //closing stmt and resultSet - Fix Fortify issue
		}finally {
				try {
					if(stmt!=null) {
						stmt.close();
					}
					if(resultSet!=null) {
						resultSet.close();
					}
				} catch (SQLException e) {
					//FIX Fortify issue
					LoggerHelper.logExceptionWithMessage(logger, "Error while closing Statement and Result Set", e);
				}
		}
	}

    public void createCsvAndZip(JSONArray tenantDBContainerArray, JSONObject tenantDBContainerCredentials,
            Object tableName, ZipOutputStream finalZos, ResultSet resultSet) {
    	CSVWriter csvWriter = null;
        try {
            String filename;
            if (tenantDBContainerArray.length() > 1) {
                filename = tenantDBContainerCredentials.getString("schema") + "." + tableName + ".csv";
            } else {
                filename = tableName + ".csv";
            }
            ZipEntry entry = new ZipEntry(filename);
            finalZos.putNextEntry(entry);

            csvWriter = new CSVWriter(new OutputStreamWriter(finalZos));
            csvWriter.writeAll(resultSet, true);
            csvWriter.flush();
            finalZos.closeEntry();

            logger.info("Data Written successfully");
        } catch (SQLException | IOException ioe) {
            logger.info("Error while writing the data");
        }
    }

    public Connection getHandler(JSONObject tenantDBContainer) {
        logger.info("Getting the DB Handler");
        Connection conn = null;
        JSONObject tenantDBContainerCredentials = tenantDBContainer.getJSONObject("credentials");
        String dbUrl = tenantDBContainerCredentials.getString("url");
        String user = tenantDBContainerCredentials.getString("user");
        String password = tenantDBContainerCredentials.getString("password");
        try {
            conn = DriverManager.getConnection(dbUrl, user, password);
            logger.info("Succesfully got the DB Handler");
        } catch (SQLException e) {
        	//FIX Fortify issue
        	LoggerHelper.logExceptionWithMessage(logger, "Error getting DB the handler", e);
        }
        return conn;
    }

    public JSONObject getTenantDBContainers(String tenantId) {
        logger.info("Get tenant DB Containers");
        RestTemplate restTemplate = new RestTemplate();
        HttpHeaders headers = new HttpHeaders();
        JSONObject serviceManagerAuthDetails = platformUtil.getCredentials(SERVICE_MANAGER);
        String accessToken = getServiceManagerJWT(serviceManagerAuthDetails).getAccessToken();
        headers.add("Authorization", "Bearer " + accessToken);
        headers.add("Content-Type", "application/json");
        String serviceManagerUrl = serviceManagerAuthDetails.getString(SERVICE_MANAGER_URL)
                + "/v1/service_bindings?labelQuery=tenant_id eq '" + tenantId + "'";
        HttpEntity<String> entity = new HttpEntity<>(headers);
        ResponseEntity<String> response = restTemplate.exchange(serviceManagerUrl, HttpMethod.GET, entity,
                String.class);
        return new JSONObject(response.getBody());
    }

    public OAuthToken getServiceManagerJWT(JSONObject serviceManagerAuthDetails) {
        logger.info("Get Service Manager JWT");
        RestTemplate restTemplate;
        String clientId = serviceManagerAuthDetails.getString(CLIENT_ID);
        String clientSecret = serviceManagerAuthDetails.getString(CLIENT_SECRET);
        String tokenUrl = serviceManagerAuthDetails.getString(AUTH_URL) + "/oauth/token?grant_type=client_credentials";
        restTemplate = new RestTemplateBuilder().basicAuthentication(clientId, clientSecret).build();
        ResponseEntity<OAuthToken> response = restTemplate.getForEntity(tokenUrl, OAuthToken.class);
        return response.getBody();
    }
}