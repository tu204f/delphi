unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdBaseComponent, IdComponent, IdUDPBase,
  IdUDPServer, Vcl.ComCtrls;

type
  TForm3 = class(TForm)
    IdUDPServer1: TIdUDPServer;
    PageControl: TPageControl;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

end.
