unit Lb.MethodFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  Lb.Create.DB,
  Lb.SysUtils,
  Lb.WinFrame;

type
  TMethodFrame = class(TFrame, IWinModule)
  private
    FMethod: TCrMethod;
    procedure SetMethod(const Value: TCrMethod);
  private
    FStatus: TStatusFrame;
    function GetCode: WideString;
    function GetStatus: TStatusFrame;
    procedure SetStatus(const AStatus: TStatusFrame);
  protected
    procedure SetApply;
    procedure SetClose;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Method: TCrMethod read FMethod write SetMethod;
    property Code: WideString read GetCode;
    property Status: TStatusFrame read GetStatus write SetStatus;
  end;

implementation

{$R *.fmx}

{ TMethodFrame }

constructor TMethodFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Align := TAlignLayout.Client;
  FMethod := nil;
end;

destructor TMethodFrame.Destroy;
begin

  inherited;
end;

procedure TMethodFrame.SetMethod(const Value: TCrMethod);
begin
  FMethod := Value;
end;

function TMethodFrame.GetCode: WideString;
begin
  Result := Self.ClassName;
end;

function TMethodFrame.GetStatus: TStatusFrame;
begin
  Result := FStatus;
end;

procedure TMethodFrame.SetStatus(const AStatus: TStatusFrame);
begin
  FStatus := AStatus;
end;

procedure TMethodFrame.SetApply;
begin
  // Применить изменение
end;

procedure TMethodFrame.SetClose;
begin
  // Отменить заявки
end;

end.
