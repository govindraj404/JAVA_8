package com.sap.ic.cmh.configuration.persistency;

import cds.gen.configurationservice.ComplaintChannels_;
import cds.gen.configurationservice.ComplaintChannels;

import com.sap.cds.Result;
import com.sap.cds.ql.Select;
import com.sap.cds.ql.cqn.CqnSelect;
import com.sap.cds.services.persistence.PersistenceService;
import com.sap.ic.cmh.utils.LoggerHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class ComplaintChannelDao {
    @Autowired
    PersistenceService db;

    private static final String COMPLAINT_CHANNEL_DAO = "ComplaintChannelDao";
    private static final Logger logger = LoggerFactory.getLogger(ComplaintChannelDao.class);


    /**
     *
     * fetch all complaint channel config order by identifier.
     *
     * @public
     */
    public Result getAllComplaintChannelsOrderByIdentifier() {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_CHANNEL_DAO, "getAllComplaintChannelsOrderByIdentifier");
        CqnSelect channelSelect = Select.from(ComplaintChannels_.class)
                .columns(ComplaintChannels.IDENTIFIER).orderBy(c -> c.get("identifier").desc());
        LoggerHelper.logMethodExit(logger, COMPLAINT_CHANNEL_DAO, "getAllComplaintChannelsOrderByIdentifier");
        return db.run(channelSelect);
    }

    /**
     *
     * fetch all complaint channel config based on ID and code columns.
     *
     * @public
     */
    public Result getAllComplaintChannelsCodeAndIDByCode(String code) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_CHANNEL_DAO, "getAllComplaintChannelsCodeAndIDByCode");
        CqnSelect channelSelect =
                Select.from(ComplaintChannels_.class).columns(ComplaintChannels.CODE, ComplaintChannels.ID)
                        .where(a -> a.code().eq(code));
        LoggerHelper.logMethodExit(logger, COMPLAINT_CHANNEL_DAO, "getAllComplaintChannelsCodeAndIDByCode");
        return db.run(channelSelect);
    }

    /**
     *
     * fetch complaint channel config based on ID.
     *
     * @public
     */
    public Result getComplaintChannelDetailsBasedOnId(String channelId) {
        LoggerHelper.logMethodEntry(logger, COMPLAINT_CHANNEL_DAO, "getComplaintChannelDetailsBasedOnId");
        return db.run(Select.from(ComplaintChannels_.class).where(b->b.ID().eq(channelId)));
    }
}
