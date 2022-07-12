namespace com.sap.ic.cmh.note;

using {
    cuid,
    managed
} from '@sap/cds/common';

using {com.sap.ic.cmh.customerComplaint.dataType as DataType} from './index';
using {com.sap.ic.cmh.noteType.NoteType} from './index';

type Note : Association to one Notes;

entity Notes : cuid, managed {
    note     : DataType.NoteText;
    noteType : NoteType;
    parentID : UUID;
}
