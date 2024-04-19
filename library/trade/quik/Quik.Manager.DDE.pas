unit Quik.Manager.DDE;

{$I quik_connect.inc}

(*******************************************************************************
    Модуль переднозначен для получение данных с трогового терминала
  QUIK посредством DDE
*******************************************************************************)

interface

uses
  Winapi.Windows,
  Winapi.DDEml,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Quik.SysUtils,
  Quik.ValueTable;

const { базовые настройки QUIK }
  SERVICE_NAME   = 'table';

(*
  Формируем запрос на системные таблицы
  procedure TQuikManagerTable.SetInitialization;

  security    - Финансовый инструменты
  trades      - Проведенные сделки
  orders      - Проведенные заявки
  stop_orders - Стоп заявка

*)


type
  TPokeAction = (paAccept, paPass, paReject);

  /// <summary>
  /// Менаджер работы с данными
  /// </summary>
  TManagerServer = class(TObject)
  private
    FServiceHSz: LongInt;
    FInst: LongInt;
    FPoke: TPokeAction;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetParser(const ANameTable: String; const AData: TBytes;
      const ASize: LongWord; ACells: String);
    property Inst: LongInt read FInst;
    property Poke: TPokeAction read FPoke;
    property ServerHSz: LongInt read FServiceHSz;
  end;

///<summary>Объект управление таблицами</summary>
function QuikManagerTable: TQuikManagerTable;

implementation

uses
{$IFDEF DEBUG}
  Lb.Logger,
{$ENDIF}
  VCL.Forms;

var
  localQuikManagerTable: TQuikManagerTable = nil;
  localManagerServer: TManagerServer = nil;

function QuikManagerTable: TQuikManagerTable;
begin
  if not Assigned(localQuikManagerTable) then
  begin
    localManagerServer := TManagerServer.Create;
    localQuikManagerTable := TQuikManagerTable.Create;
  end;
  Result := localQuikManagerTable;
end;

/// <summary>
/// Обратное функция для получение данных
/// </summary>
function CallbackProc(CallType, Fmt: UINT; Conv: HConv; hsz1, hsz2: HSZ;
  Data: HDDEData; Data1, Data2: ULONG_PTR): HDDEData stdcall;
const
  COUNT_BUFFER = 255;
var
  mSize: Integer;
  mData: TBytes;
  TableName, Cells: String;
  Buffer: array [0 .. COUNT_BUFFER] of Char;

begin
  Result := 0;
  case CallType of
    XTYP_CONNECT: begin
      Result := 1;
    end;
    XTYP_POKE:
      begin
        DdeQueryString(localManagerServer.Inst, hsz1, Buffer, COUNT_BUFFER,CP_WINUNICODE);
        TableName := Trim(String(Buffer));
        case localManagerServer.Poke of
          paAccept:
            begin
              { Получено таблицы }
              DdeQueryString(localManagerServer.Inst, hsz2, Buffer, COUNT_BUFFER,CP_WINUNICODE);
              Cells := String(Buffer);

              mSize := DdeGetData(Data, nil, 4, 0);
              SetLength(mData,mSize);
              DdeGetData(Data, mData, mSize, 0);
              { отсортировать данные по таблицам }
              localManagerServer.SetParser(TableName,mData,mSize,Cells);
              Result := DDE_FACK;
            end;
          paPass:
            Result := DDE_FACK; // Пропустить данные
          paReject:
            Result := DDE_FNOTPROCESSED; // Все данные больше не нужно
        end;
      end;
  end;
end;

function GetPathApp: String;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

{ TManagerServer }

constructor TManagerServer.Create;
var
  iResult: Integer;
begin
  FPoke := paAccept;
  FInst := 0;
  iResult := DdeInitialize(FInst,CallbackProc, APPCLASS_STANDARD, 0);
  if iResult = DMLERR_NO_ERROR then begin
    { регестрируем сервер }
    FServiceHSz := DdeCreateStringHandle(FInst,PChar(SERVICE_NAME),CP_WINUNICODE);
    { регестрируем таблицы }
    { security,trade,order,holding,position }
    // DdeCreateStringHandle(FInst,PChar(TABLE_SECURITY),CP_WINUNICODE);
    // DdeCreateStringHandle(FInst,PChar(TABLE_HOLDING),CP_WINUNICODE);
    // DdeCreateStringHandle(FInst,PChar(TABLE_POSITION),CP_WINUNICODE);

    iResult := DdeNameService(FInst, FServiceHSz, 0, DNS_REGISTER);
    if iResult = 0 then
      raise Exception.Create('Не удалось зарегистрировать имя ' +
        'DDE-сервиса ''' + SERVICE_NAME + '''');
  end else
    raise Exception.Create('Не удалось выполнить инициализацию сервера - DDE');
end;

destructor TManagerServer.Destroy;
begin
  if (Self.Inst > 0) and (Self.ServerHSz > 0) then
  begin
    DdeNameService(FInst, FServiceHSz, 0, DNS_UNREGISTER);
    DdeFreeStringHandle(FInst, FServiceHSz);
    DdeUninitialize(FInst);
  end;
  inherited;
end;

procedure TManagerServer.SetParser(const ANameTable: String;
  const AData: TBytes; const ASize: LongWord; ACells: String);

  procedure SetCell(const ACell: String; var ARow, ACol: Integer);
  var
    xIndex: Integer;
  begin
    xIndex := Pos('C',ACell);
    ARow := StrToIntDef(Copy(ACell,2,xIndex - 2),0);
    ACol := StrToIntDef(Copy(ACell,xIndex + 1,Length(ACell) - xIndex),0);
  end;

  procedure SetCells(const ACells: String; var ARow1, ACol1, ARow2, ACol2: Integer);
  var
    xIndex: Integer;
    xCl1, xCl2: String;
  begin
    xIndex := Pos(':',ACells);
    xCl1 := Copy(ACells,1,xIndex - 1);
    xCl2 := Copy(ACells,xIndex + 1,Length(ACells) - xIndex);
    SetCell(xCl1,ARow1,ACol1);
    SetCell(xCl2,ARow2,ACol2);
  end;

var
  xRow1, xCol1, xRow2, xCol2: Integer;
  xValueBlock: TValueBlock;
begin
  SetCells(ACells, xRow1, xCol1, xRow2, xCol2);

  xValueBlock.Data := AData;
  xValueBlock.Size := ASize;
  xValueBlock.NameTable := ANameTable;
  xValueBlock.Row1 := xRow1;
  xValueBlock.Row2 := xRow2;
  xValueBlock.Col1 := xCol1;
  xValueBlock.Col2 := xCol2;
  {$IFDEF LOG_QUIK_TABLE}
  TLogger.LogForm('table',ANameTable + '; size = ' + IntToStr(ASize) + '; cells [' + ACells + ']');
  {$ENDIF}
  QuikManagerTable.SetValueBlock(xValueBlock);
end;

initialization
  QuikManagerTable;

finalization
  FreeAndNil(localQuikManagerTable);
  FreeAndNil(localManagerServer);

end.
