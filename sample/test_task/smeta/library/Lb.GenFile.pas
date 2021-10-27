unit Lb.GenFile;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  Lb.SysUtils;

type
  ///<summary>Поток – который генерирует файл</summary>
  TGenFile = class(TBaseThread)
  public const
    SIZE_MG = 1048576;
    SIZE_GB = 1073741824;
  private
    FSizeFile: Integer;
    FTypeSizeFile: Integer;
    FNumberForm: Integer;
    FNumberTo: Integer;
    FReferences: TStrings;
    FFileName: String;
  private
    FPorgress: Int64;
    FMaxSize: Int64;
  protected
    function GetMaxSizeFile: Int64;
    procedure DoWork; override;
  public
    constructor Create;
    destructor Destroy; override;
    property SizeFile: Integer read FSizeFile write FSizeFile;
    property TypeSizeFile: Integer read FTypeSizeFile write FTypeSizeFile;
    property NumberForm: Integer read FNumberForm write FNumberForm;
    property NumberTo: Integer read FNumberTo write FNumberTo;
    property References: TStrings read FReferences;
    property FileName: String read FFileName write FFileName;
  public
    property Progress: Int64 read FPorgress;
    property MaxSize: Int64 read FMaxSize;
  end;

implementation

{ TGenFile }

constructor TGenFile.Create;
begin
  inherited Create;
  FReferences := TStringList.Create;
end;

destructor TGenFile.Destroy;
begin
  FreeAndNil(FReferences);
  inherited;
end;

function TGenFile.GetMaxSizeFile: Int64;
begin
  Result := 0;
  case FTypeSizeFile of
    0: Result := FSizeFile * TGenFile.SIZE_MG;
    1: Result := FSizeFile * TGenFile.SIZE_GB;
  end;
end;

procedure TGenFile.DoWork;

  function GetGenNumber: Integer;
  var
    xDelta: Integer;
  begin
    xDelta := FNumberTo - FNumberForm;
    Result := FNumberForm + Random(xDelta + 1);
  end;

  function GetGetText: String;
  var
    xIndex: Integer;
  begin
    xIndex := Random(FReferences.Count);
    Result := FReferences[xIndex];
  end;

var
  xV: TParamValue;
  xB: TBytes;
  xFileStream: TFileStream;
  xPositionStream: Int64;
  xMaxSizeFile: Int64;
begin
  if (FNumberForm <= 0) or (FNumberTo <= 0) or (FNumberForm = FNumberTo) then
    raise Exception.Create('Error Message: Максимальный размер файла не определен: ' + xMaxSizeFile.ToString);

  if FReferences.Count = 0 then
    raise Exception.Create('Error Message: Не загружен справочник текстовых сообщений: ' + xMaxSizeFile.ToString);

  xMaxSizeFile := Self.GetMaxSizeFile;
  xFileStream := TFileStream.Create(FFileName,fmCreate);
  try


    FMaxSize := xMaxSizeFile;

    xPositionStream := 0;
    while xPositionStream < xMaxSizeFile do
    begin
      if Self.Terminated then
        Break;

      xV.Number := GetGenNumber;
      xV.Text := GetGetText;
      xB := TEncoding.UTF8.GetBytes(xV.Value + sLineBreak);
      xFileStream.Position := xPositionStream;
      xFileStream.Write(xB,Length(xB));

      FPorgress := xPositionStream;
      xPositionStream := xPositionStream + Length(xB);
    end;
  finally
    FreeAndNil(xFileStream);
  end;
end;


end.
