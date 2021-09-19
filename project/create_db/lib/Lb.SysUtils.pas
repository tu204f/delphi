unit Lb.SysUtils;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

type
  TStatusFrame = (
    fsCreate,  // Создание данных
    fsChange   // Изменение формы
  );

resourcestring  {Событие формы, куда буду помещать frame}
  EVENT_WIN_FRAME_APPLY       = 'win_frame_apply';
  EVENT_WIN_FRAME_CLOSE       = 'win_frame_close';

  EVENT_MODULE_ADD            = 'module_add';
  EVENT_MODULE_CHANGE         = 'module_change';
  EVENT_MODULE_DELETED        = 'module_deleted';

  EVENT_MODULE_TABLE_UPDATA   = 'module_table_updata';
  EVENT_MODULE_TABLE_DLCLICK  = 'module_table_dlclick';

  EVENT_DOMAIN_TABLE_ADD      = 'domain_table_add';
  EVENT_DOMAIN_TABLE_CHANGE   = 'domain_table_change';
  EVENT_DOMAIN_TABLE_DELETED  = 'domain_table_deleted';


///<summary>Генерирование ключа объекта</summary>
///<remarks>Размер ключа, генерируемого</remarks>
function GetCreateObjectKey: String;

implementation

function GetCreateGUID: String;

  function GetGeneratorCode: String;
  var
    xC, tmpS: String;
    i: Integer;
  begin
    tmpS := '';
    for i := 1 to 13 do
    begin
      xC := IntToHex(Random(16),1);
      tmpS := tmpS + xC;
    end;
    Result := tmpS;
  end;

var
  xGUID : TGUID;
  xCode, xS: String;
begin
  xCode := GetGeneratorCode;
  CreateGUID(xGUID);
  xS := GUIDToString(xGUID);
  Result := Copy(xS,2,xS.Length - 2) + '-' + xCode;
end;

function GetCreateObjectKey: String;
begin
  Result := GetCreateGUID;
end;

end.
