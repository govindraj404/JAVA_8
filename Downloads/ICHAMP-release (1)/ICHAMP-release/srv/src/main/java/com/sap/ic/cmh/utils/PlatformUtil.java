package com.sap.ic.cmh.utils;

import io.pivotal.cfenv.core.CfEnv;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.apache.http.ParseException;
import org.slf4j.Logger;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class PlatformUtil {
    public static final Logger logger = LoggerHelper.getLogger(PlatformUtil.class);

    public String getEnvironmentDetail() {
        return System.getenv("VCAP_SERVICES");
    }

    /*
     * Get reusable service env variables from vcap service
     */
    public JSONObject getCredentials(String service) {
        String vcapServices = getEnvironmentDetail();
        JSONObject credential = new JSONObject();
        try {
            JSONObject vcapServiceObject = new JSONObject(vcapServices);
            JSONArray vcapServiceArray = vcapServiceObject.getJSONArray(service);
            if (vcapServiceArray != null) {
                JSONObject vcapServiceArrayObject = vcapServiceArray.getJSONObject(0);
                credential = vcapServiceArrayObject.getJSONObject("credentials");
            }
        } catch (JSONException e) {
            logger.error("JSONException");
        } catch (ParseException e) {
            logger.error("ParseException");
        }
        return credential;
    }

    /*
     * Get user provided instance env variables from vcap service
     */
    public JSONObject getUserProvidedInstanceCredentials(String service) {
        String vcapServices = getEnvironmentDetail();
        JSONObject credential = new JSONObject();
        try {
            JSONObject vcapServiceObject = new JSONObject(vcapServices);
            JSONArray vcapServiceArray = vcapServiceObject.getJSONArray("user-provided");
            if (vcapServiceArray != null) {
                for (int i = 0; i < vcapServiceArray.length(); i++) {
                    if (vcapServiceArray.getJSONObject(i).getString("name").equals(service)) {
                        JSONObject vcapServiceArrayObject = vcapServiceArray.getJSONObject(i);
                        credential = vcapServiceArrayObject.getJSONObject("credentials");
                    }
                }
            }
        } catch (JSONException e) {
            logger.error("JSONException");
        } catch (ParseException e) {
            logger.error("ParseException");
        }
        return credential;
    }

    public Map<String, Object> getServiceCredentials(String serviceName) {
        CfEnv cfEnv = new CfEnv();
        return cfEnv.findCredentialsByLabel(serviceName).getMap();
    }
}
