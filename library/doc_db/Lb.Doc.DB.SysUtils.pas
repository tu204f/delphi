unit Lb.Doc.DB.SysUtils;

interface

uses
  Lb.Doc.DB;

implementation

(******************************************************************************)
(* ��������� ��� ������ � API                                                 *)
(******************************************************************************)

function OpenDataBase(const ANameFile: String): TDocFile;
begin
  /// ������� ���� ������
end;

function CloseDataBase(const ADataBaseID: Integer): Boolean;
begin
  // ������� ���� ������
  Result := False;
end;


(******************************************************************************)
(* �������� ��� ������ � �������                                              *)
(******************************************************************************)

function GetNewFolder(const ADocFile: TDocFile; const ANewNameFolder: String): TFolder;
begin
  // � ������ � ������� �����, ����� � ������ ANewNameFolder
  Result := nil;
end;

function GetReNameFolder(const ADocFile: TDocFile; const ANewNameFolder, AOldNameFolder: String): TFolder;
begin
  // ���� ��������� �����
  Result := nil;
end;

function GetDeleteFolder(const ADocFile: TDocFile; const ANewNameFolder: String): Boolean;
begin
  Result := False;
end;

// �������
// ������ �����


(******************************************************************************)
(* �������� ��� ������ � ����������                                           *)
(******************************************************************************)



end.
