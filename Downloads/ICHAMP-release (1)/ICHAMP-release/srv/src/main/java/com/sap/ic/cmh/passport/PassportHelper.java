package com.sap.ic.cmh.passport;

import com.sap.jdsr.passport.DSRPassport;
import java.nio.ByteBuffer;
import java.util.Optional;
import java.util.UUID;

import com.sap.jdsr.passport.EncodeDecode;
import com.sap.jdsr.util.ConvertHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class PassportHelper {

    /** The version of the SAP Passport. Guide recommended this number. */
    private static final int VERSION = 3;

    /** This is only relevant for trace use cases. */
    private static final int TRACE_FLAG = 0x0000;

    /** Name of the component which has created the Passport. */
    public static final String COMPONENT_NAME = "cmh";

    /** Name of component specific SERVICE // has its origin from ABAP not relevant */
    private static final int SERVICE = 0;

    /**
     * The name of component specific action type, see coding in some table inside the SAP passport
     * ID documentation PDF, 11 means HTTP
     */
    private static final int ACTION_TYPE = 11;

    /** The name of client in business system has its origin from ABAP, not relevant */
    private static final String CLIENT_NUMBER = "   ";

    /**
     * The type of initial component : see coding in some table inside the SAP passport ID
     * documentation PDF, 41 means Industry Cloud.
     */
    private static final int COMPONENT_TYPE = 41;

    private PassportHelper(){
    }
    /**
     * provided by Claudia Schmidt
     *
     * @param uuid {@link UUID}
     * @return byte array
     */
    public static byte[] getBytesFromUUID(UUID uuid) {
        ByteBuffer bb = ByteBuffer.wrap(new byte[16]);
        bb.putLong(uuid.getMostSignificantBits());
        bb.putLong(uuid.getLeastSignificantBits());

        return bb.array();
    }

    /**
     * Creates a fresh (local) SAP Passport.
     *
     * @param action the action which initiates the creation
     * @return {@link DSRPassport} which has been created
     */
    public static DSRPassport createLocalPassport(String action, String userID) {
        // identifies the initial context within a complex scenario e.g. the initiating user
        // session, like a session id
        final byte[] rootContextId = getBytesFromUUID(UUID.randomUUID());

        final String transactionID = ConvertHelper.byteArrayToHex(getBytesFromUUID(UUID.randomUUID()));

        final byte[] connectionId = getBytesFromUUID(UUID.randomUUID());

        // Identifies call per connection, should start with 1 and be incremented.
        final int connectionCounter = 1;

        DSRPassport passport =
                new DSRPassport(
                        VERSION,
                        TRACE_FLAG,
                        COMPONENT_NAME,
                        SERVICE,
                        userID,
                        action,
                        ACTION_TYPE,
                        COMPONENT_NAME,
                        transactionID,
                        CLIENT_NUMBER,
                        COMPONENT_TYPE,
                        rootContextId,
                        connectionId,
                        connectionCounter);
        log.info("Created new local passport: '{}'.", PassportHelper.createPassportHexString(passport));
        return passport;
    }

    /**
     * Creates a copy (remote) of origin SAP Passport for outbound communication.
     *
     * @param passport {@link DSRPassport}
     * @return {@link DSRPassport} which has been created
     */
    public static DSRPassport createRemotePassport(DSRPassport passport) {
        DSRPassport remotePassport = new DSRPassport();
        remotePassport.setPassport(passport);
        remotePassport.setConnectionIdBytesInternal(getBytesFromUUID(UUID.randomUUID()));
        remotePassport.setConnectionCounterInternal(1);
        remotePassport.setPrevSystemIdInternal(PassportHelper.COMPONENT_NAME);
        log.info("Created new remote passport: '{}'.", PassportHelper.createPassportHexString(remotePassport));

        return remotePassport;
    }

    public static String createPassportHexString(DSRPassport passport) {
        return ConvertHelper.byteArrayToHex(passport.getNetPassport());
    }

    public static Optional<DSRPassport> readPassportFromHexString(String hexValue) {
        if (hexValue != null) {
            return Optional.of(EncodeDecode.decodeBytePassport(ConvertHelper.hexToByteArray(hexValue)));
        }

        return Optional.empty();
    }
}
