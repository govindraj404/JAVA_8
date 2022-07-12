namespace com.sap.ic.cmh.noteType;

using {com.sap.ic.cmh.customerComplaint.dataType as DataType} from './index';
using {com.sap.ic.cmh.configuration.dataType as ConfigurationDataType} from '../configuration/index';

type NoteType : Association to one NoteTypes;

entity NoteTypes {
    key code               : DataType.NoteTypeCode;
        description        : localized DataType.NoteTypeDescription;
        isExternalRelevant : ConfigurationDataType.Flag;
}
