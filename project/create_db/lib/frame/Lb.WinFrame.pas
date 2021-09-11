unit Lb.WinFrame;

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
  FMX.Objects,
  FMX.Layouts,
  FMX.Controls.Presentation,
  Lb.Create.DB,
  Lb.SysUtils;

type
  IWinModule = interface
  ['{471730EF-7CEB-45EE-88A1-0B3D85825FE2}']
  {private}
    function GetCode: WideString;
    function GetStatus: TStatusFrame;
    procedure SetStatus(const AStatus: TStatusFrame);
  {protected}
    procedure SetApply;
    procedure SetClose;
  {public}
    property Code: WideString read GetCode;
    property Status: TStatusFrame read GetStatus write SetStatus;
  end;

  TWinFrame = class(TFrame)
    RectangleF: TRectangle;
    RectangleW: TRectangle;
    LayoutBottom: TLayout;
    ButtonClose: TButton;
    ButtonApply: TButton;
    LayoutObject: TLayout;
    LayoutTop: TLayout;
    TitleText: TText;
    Line: TLine;
    LineBottom: TLine;
    procedure ButtonApplyClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  private
    FClassCode: String;
    FWin: TFrame;
    function GetTitle: String;
    procedure SetTitle(const Value: String);
    procedure SetWin(const Value: TFrame);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Title: String read GetTitle write SetTitle;
    property Win: TFrame  read FWin write SetWin;
    property ClassCode: String read FClassCode;
  end;

implementation

{$R *.fmx}

uses
  Lb.Core.Events;

constructor TWinFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TWinFrame.Destroy;
begin
  if Assigned(FWin) then
  begin
    FreeAndNil(FWin);
    FWin := nil;
  end;
  inherited;
end;

function TWinFrame.GetTitle: String;
begin
  Result := TitleText.Text;
end;

procedure TWinFrame.SetTitle(const Value: String);
begin
  TitleText.Text := Value;
end;

procedure TWinFrame.SetWin(const Value: TFrame);
begin
  FWin := Value;
  if Assigned(FWin) then
  begin
    FWin.Parent := LayoutObject;
    FWin.Align := TAlignLayout.Client;
    FClassCode := FWin.ClassName;
  end;
end;

procedure TWinFrame.ButtonApplyClick(Sender: TObject);
var
  xStr: TStrings;
  xWinFrame: IWinModule;
begin
  if FWin.GetInterface(IWinModule,xWinFrame) then
    xWinFrame.SetApply;

  xStr := TStringList.Create;
  try
    xStr.Values['class_name'] := FClassCode;
    xStr.Values['status'] := '0';
    ApplicationEvents.SetEvent(EVENT_WIN_FRAME_APPLY,Self,xStr);
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TWinFrame.ButtonCloseClick(Sender: TObject);
var
  xStr: TStrings;
  xWinFrame: IWinModule;
begin
  if FWin.GetInterface(IWinModule,xWinFrame) then
    xWinFrame.SetClose;

  xStr := TStringList.Create;
  try
    xStr.Values['class_name'] := FClassCode;
    xStr.Values['status'] := '-1';
    ApplicationEvents.SetEvent(EVENT_WIN_FRAME_CLOSE,Self);
  finally
    FreeAndNil(xStr);
  end;
end;


end.
