(*******************************************************************************
  Расширяем функционал - объекта, дополняем TParams
  с возможность дабовление генерации данных
*******************************************************************************)
unit Lb.ListBoxItem.Params;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Objects,
  FMX.Types,
  FMX.Forms,
  FMX.Layouts,
  FMX.StdCtrls,
  FMX.ListBox;

type
  TListBoxHeaderParams = class(TListBoxHeader)
  private
    FFrame: TFrame;
    FParams: TStrings;
    function GetParams(Name: String): String;
    procedure SetParams(Name: String; const Value: String);
  protected
    function GetRowCreateFrame: TFrame; virtual;
    function GetDefaultStyleLookupName: string; override;
    procedure SetPositionFrame; virtual;
    procedure SetFrame(const Value: TFrame); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Params[Name: String]: String read GetParams write SetParams;
    property Frame: TFrame read FFrame;
  end;

  TListBoxItemParams = class(TListBoxItem)
  public const
    BOX_ITEM_HIGH = 27;
  private
    FFrame: TFrame;
    FParams: TStrings;
    function GetParams(Name: String): String;
    procedure SetParams(Name: String; const Value: String);
  protected
    function GetRowCreateFrame: TFrame; virtual;
    function GetDefaultStyleLookupName: string; override;
    procedure SetPositionFrame; virtual;
    procedure SetFrame(const Value: TFrame); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Strings: TStrings read FParams;
    property Params[Name: String]: String read GetParams write SetParams;
    property Frame: TFrame read FFrame write SetFrame;
  end;

  ///<summary>Item – с  Rectangle</summary>
  TListBoxItemRectangle = class(TListBoxItemParams)
  private
    FText: TText;
    FRectangle: TRectangle;
    FCloseButton: TButton;
  private
    FOnCloseButton: TNotifyEvent;
    procedure CloseButtonOnClick(Sender: TObject);
  protected
    procedure SetText(const Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Rectangle: TRectangle read FRectangle;
    property OnCloseButton: TNotifyEvent write FOnCloseButton;
  end;

implementation

{ TListBoxHeaderParams }

constructor TListBoxHeaderParams.Create(AOwner: TComponent);
var
  xFrame: TFrame;
begin
  inherited Create(AOwner);
  FParams := TStringList.Create;
  xFrame := GetRowCreateFrame;
  SetFrame(xFrame);
end;

destructor TListBoxHeaderParams.Destroy;
begin
  if Assigned(FFrame) then
    FreeAndNil(FFrame);
  FreeAndNil(FParams);
  inherited;
end;

function TListBoxHeaderParams.GetDefaultStyleLookupName: string;
begin
  // Result := 'ListBoxItemstyle';
  Result := inherited GetDefaultStyleLookupName;
end;

function TListBoxHeaderParams.GetRowCreateFrame: TFrame;
begin
  Result := nil;
end;

procedure TListBoxHeaderParams.SetFrame(const Value: TFrame);
begin
  FFrame := Value;
  if Assigned(FFrame) then
  begin
    SetPositionFrame;
    FFrame.Parent := Self;
  end;
end;

procedure TListBoxHeaderParams.SetPositionFrame;
var
  xFrame: TFrame;
begin
  xFrame := Self.Frame;
  if Assigned(xFrame) then
  begin
    xFrame.Align := TAlignLayout.Client;
    with xFrame.Margins do
    begin
      Left := 0;
      Right := 0;
      Top := 0;
      Bottom := 2;
    end;
  end;
end;

function TListBoxHeaderParams.GetParams(Name: String): String;
begin
  Result := FParams.Values[Name];
end;

procedure TListBoxHeaderParams.SetParams(Name: String; const Value: String);
begin
  FParams.Values[Name] := Value;
end;

{ TListBoxItemParams }

constructor TListBoxItemParams.Create(AOwner: TComponent);
var
  xFrame: TFrame;
begin
  inherited Create(AOwner);
  Self.Height := BOX_ITEM_HIGH;
  FParams := TStringList.Create;
  xFrame := GetRowCreateFrame;
  SetFrame(xFrame);
end;

destructor TListBoxItemParams.Destroy;
begin
  if Assigned(FFrame) then
    FreeAndNil(FFrame);
  FreeAndNil(FParams);
  inherited;
end;

function TListBoxItemParams.GetDefaultStyleLookupName: string;
begin
  // Result := 'ListBoxItemstyle';
  Result := inherited GetDefaultStyleLookupName;
end;

function TListBoxItemParams.GetRowCreateFrame: TFrame;
begin
  Result := nil;
end;

procedure TListBoxItemParams.SetPositionFrame;
var
  xFrame: TFrame;
begin
  xFrame := Self.Frame;
  if Assigned(xFrame) then
  begin
    xFrame.Align := TAlignLayout.Client;
    with xFrame.Margins do
    begin
      Left := 0;
      Right := 0;
      Top := 0;
      Bottom := 2;
    end;
  end;
end;

procedure TListBoxItemParams.SetFrame(const Value: TFrame);
begin
  FFrame := Value;
  if Assigned(FFrame) then
  begin
    SetPositionFrame;
    FFrame.Parent := Self;
  end;
end;

function TListBoxItemParams.GetParams(Name: String): String;
begin
  Result := FParams.Values[Name];
end;

procedure TListBoxItemParams.SetParams(Name: String; const Value: String);
begin
  FParams.Values[Name] := Value;
end;



{ TListBoxItemRectangle }

constructor TListBoxItemRectangle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FRectangle := TRectangle.Create(Self);
  FRectangle.Parent := Self;
  FRectangle.Align := TAlignLayout.Client;
  with FRectangle do
  begin
    Margins.Left := 3;
    Margins.Right := 3;
    Margins.Top := 1.5;
    Margins.Bottom := 1.5;
  end;
  FRectangle.HitTest := False;
  FRectangle.Fill.Color := TAlphaColorRec.White;

  // Иницилизация текста
  FText := TText.Create(Self);
  FText.Parent := FRectangle;
  FText.Align := TAlignLayout.Client;
  with FText.TextSettings do
  begin
    HorzAlign := TTextAlign.Leading;
  end;

  with FText do
  begin
    Margins.Left := 3;
    Margins.Right := 3;
    Margins.Top := 1.5;
    Margins.Bottom := 1.5;
  end;
  FText.HitTest := False;

  // Инициализация кнопки
  FCloseButton := TButton.Create(Self);
  FCloseButton.Parent := FRectangle;
  FCloseButton.Align := TAlignLayout.Right;
  FCloseButton.OnClick := CloseButtonOnClick;
  FCloseButton.Text := 'Удалить параметр';
  FCloseButton.Width := 150;

  with FCloseButton do
  begin
    Margins.Right := 3;
    Margins.Top := 3;
    Margins.Bottom := 3;
  end;

end;

destructor TListBoxItemRectangle.Destroy;
begin
  FreeAndNil(FText);
  FreeAndNil(FRectangle);
  inherited;
end;

procedure TListBoxItemRectangle.SetText(const Value: string);
begin
  inherited SetText(Value);
  if Assigned(FText) then
    FText.Text := Value;
end;

procedure TListBoxItemRectangle.CloseButtonOnClick(Sender: TObject);
begin
  if Assigned(FOnCloseButton) then
    FOnCloseButton(Self);
end;

end.
