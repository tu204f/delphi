unit Lb.SysUtils.StructureFile;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils.Candel;


procedure SetStructureFile(const APath: String; const AStructure: TStructure);

implementation

var
  localIndex: Integer = 0;

const
  SQL_CREATE_PATERN =
    'create table if not exists patern (' +
    ' _id text primary key,     ' +   // ���������� ���� ���������
    ' ind   integer,            ' +   // ���������� ����� � �������
    ' deleted integer default 0,' +   // ��������� ������
    ' status integer default 0  ' +   // ������
    ');';

  SQL_CREATE_INDEX_PK_PATERN =
    'create unique index if not exists index_pk_patern on patern(_id);';

  SQL_CREATE_PATERN_VALUES =
    'create table if not exists patern_values (' +
    ' _id text primary key,     ' +   // ���������� ���� ���������
    ' _patern_id text,          ' +   // ������ �� ���������
    ' id    integer,            ' +   // ���������� �����, � �������
    ' open  double,             ' +   // ���� ��������
    ' high  double,             ' +   // ������������ ����
    ' low   double,             ' +   // ����������� ����
    ' close double,             ' +   // ���� ��������
    ' vol   double,             ' +   // ����� ������� ������
    ' deleted integer default 0,' +   // ��������� ������
    ' status integer default 0, ' +   // ������ {TTypeValue}
                                      //  1 - �������� ������
                                      //  2 - ��������
    ' constraint fk_patern         ' +
    '  foreign key (_patern_id)    ' +
    '  references patern(_id)      ' +
    '  on delete cascade           ' +

    ');';






procedure SetStructureFile(const APath: String; const AStructure: TStructure);

  function _ToInd(const AValue: Integer): String;
  begin
    if (AValue >= 0) and (AValue <= 9) then
      Result := '00' + IntToStr(AValue)
    else if (AValue >= 10) and (AValue <= 99) then
      Result := '0' + IntToStr(AValue)
    else
      Result := IntToStr(AValue);
  end;

  procedure _CandelInfo(
    const ACaption: String;
    const AStrings: TStrings;
    const ACandels: TCandelList
  );
  var
    i, iCount: Integer;
    xC: TCandel;
  begin
    AStrings.Add('[' + ACaption + ']');
    iCount := ACandels.Count;
    for i := 0 to iCount - 1 do
    begin
      xC := ACandels[i];
      AStrings.Add('v_' + _ToInd(i) + '=' + xC.ToStringCandel);
    end;
  end;

var
  xStr: TStrings;
  xFileName: String;
begin
  Inc(localIndex);
  xStr := TStringList.Create;
  try
    _CandelInfo('source',xStr,AStructure.SourceVectors);
    _CandelInfo('future',xStr,AStructure.FutureVectors);
  finally
    xFileName := APath + 'patern_' + IntToStr(localIndex) + '.ini';
    xStr.SaveToFile(xFileName);
    FreeAndNil(xStr);
  end;
end;

end.
