/*
 *  libgfx2 - FreeBASIC's alternative gfx library
 *	Copyright (C) 2005 Angelo Mottola (a.mottola@libero.it)
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/*
 * win32.c -- common win32 routines and drivers list
 *
 * chng: jan/2005 written [lillo]
 *
 */

#include "fb_gfx.h"
#include "fb_gfx_win32.h"
#include <process.h>
#include <stdio.h>
#include <assert.h>


WIN32DRIVER fb_win32;

const GFXDRIVER *fb_gfx_driver_list[] = {
#ifndef TARGET_CYGWIN
    &fb_gfxDriverDirectDraw,
#endif
	&fb_gfxDriverGDI,
	&fb_gfxDriverOpenGL,
	NULL
};


static CRITICAL_SECTION update_lock;
static HANDLE handle;
static BOOL screensaver_active, cursor_shown;
static UINT msg_cursor;
static int mouse_buttons, mouse_wheel;


static void ToggleFullScreen( void )
{
    if (fb_win32.flags & DRIVER_NO_SWITCH)
        return;

    fb_win32.exit();
    fb_win32.flags ^= DRIVER_FULLSCREEN;
    if (fb_win32.init()) {
        fb_win32.exit();
        fb_win32.flags ^= DRIVER_FULLSCREEN;
        fb_win32.init();
    }
    fb_hRestorePalette();
    fb_hMemSet(fb_mode->dirty, TRUE, fb_win32.h);
    fb_win32.is_active = TRUE;
}

/*:::::*/
LRESULT CALLBACK fb_hWin32WinProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	unsigned char key_state[256];

	if (message == msg_cursor) {
		ShowCursor(wParam);
		return FALSE;
	}

	switch (message)
	{
		case WM_ACTIVATEAPP:
			fb_win32.is_active = (int)wParam;
			fb_hMemSet(fb_mode->key, FALSE, 128);
			mouse_buttons = 0;
			fb_hMemSet(fb_mode->dirty, TRUE, fb_win32.h);
			break;

		case WM_LBUTTONDOWN:
			mouse_buttons |= 0x1;
			break;

		case WM_LBUTTONUP:
			mouse_buttons &= ~0x1;
			break;

		case WM_RBUTTONDOWN:
			mouse_buttons |= 0x2;
			break;

		case WM_RBUTTONUP:
			mouse_buttons &= ~0x2;
			break;

		case WM_MBUTTONDOWN:
			mouse_buttons |= 0x4;
			break;

		case WM_MBUTTONUP:
			mouse_buttons &= ~0x4;
			break;

		case WM_MOUSEWHEEL:
			if (fb_win32.version < 0x40A)
				break;
			if ((signed)wParam > 0)
				mouse_wheel++;
			else
				mouse_wheel--;
			break;

		case WM_SIZE:
        case WM_SYSKEYDOWN:
            if (!fb_win32.is_active)
                break;
            {
                int is_alt_enter = ((message == WM_SYSKEYDOWN) && (wParam == VK_RETURN) && (lParam & 0x20000000));
                int is_maximize = ((message == WM_SIZE) && (wParam == SIZE_MAXIMIZED));
                if ( is_maximize || is_alt_enter) {
                    ToggleFullScreen();
                    return FALSE;
                }
                if( message!=WM_SYSKEYDOWN || wParam!=VK_F10 )
                    break;
            }
            /* fall thru to WM_KEYDOWN because we want F10 */

        case WM_KEYDOWN:
            /* Beware of the fall-thru from WM_SYSKEYDOWN */
			if (!fb_win32.is_active)
                break;

            {
                WORD wVkCode = (WORD) wParam;
                WORD wVsCode = (WORD) (( lParam & 0xFF0000 ) >> 16);
                int is_ext_keycode = ( lParam & 0x1000000 )!=0;
                size_t repeat_count = ( lParam & 0xFFFF );
                DWORD dwControlKeyState = 0;
                int key;

                GetKeyboardState(key_state);
                if( (key_state[VK_SHIFT] || key_state[VK_LSHIFT] || key_state[VK_RSHIFT]) & 0x80 )
                    dwControlKeyState ^= SHIFT_PRESSED;
                if( (key_state[VK_LCONTROL] || key_state[VK_CONTROL]) & 0x80 )
                    dwControlKeyState ^= LEFT_CTRL_PRESSED;
                if( key_state[VK_RCONTROL] & 0x80 )
                    dwControlKeyState ^= RIGHT_CTRL_PRESSED;
                if( (key_state[VK_LMENU] || key_state[VK_MENU]) & 0x80 )
                    dwControlKeyState ^= LEFT_ALT_PRESSED;
                if( key_state[VK_RMENU] & 0x80 )
                    dwControlKeyState ^= RIGHT_ALT_PRESSED;
                if( is_ext_keycode )
                    dwControlKeyState |= ENHANCED_KEY;

                key =
                    fb_hConsoleTranslateKey( 0,
                                             wVsCode,
                                             wVkCode,
                                             dwControlKeyState,
                                             TRUE );
                if (key!=-1) {
                    while( repeat_count-- ) {
                        fb_hPostKey(key);
                    }
                }

                /* We don't want to enter the menu ... */
                if( wVkCode==VK_F10 )
                    return FALSE;
            }
            break;

        case WM_CHAR:
            {
                size_t repeat_count = ( lParam & 0xFFFF );
                int key = (int) wParam;
                if( key < 256 ) {
                    int target_cp = FB_GFX_GET_CODEPAGE();
                    if( target_cp!=-1 ) {
                        char ch[2] = {
                            (char) key,
                            0
                        };
                        FBSTRING *result =
                            fb_StrAllocTempDescF( ch, 2 );
                        result = fb_hIntlConvertString( result,
                                                        CP_ACP,
                                                        target_cp );
                        key = (unsigned) (unsigned char) result->data[0];
                    }

                    while( repeat_count-- ) {
                        fb_hPostKey(key);
                    }
                }
            }

            break;

		case WM_CLOSE:
			fb_hPostKey(KEY_QUIT);
			return FALSE;

		case WM_PAINT:
			fb_win32.paint();
			break;
	}

	return DefWindowProc(hWnd, message, wParam, lParam);
}


/*:::::*/
void fb_hHandleMessages(void)
{
	MSG message;

	while (PeekMessage(&message, fb_win32.wnd, 0, 0, PM_REMOVE)) {
		TranslateMessage(&message);
		DispatchMessage(&message);
	}
}


/*:::::*/
int fb_hWin32Init(char *title, int w, int h, int depth, int refresh_rate, int flags)
{
	OSVERSIONINFO info;
	HANDLE events[2];
	long result;

	info.dwOSVersionInfoSize = sizeof(info);
	GetVersionEx(&info);
	fb_win32.version = (info.dwMajorVersion << 8) | info.dwMinorVersion;

	msg_cursor = RegisterWindowMessage("FB mouse cursor");
	cursor_shown = TRUE;

	SystemParametersInfo(SPI_GETSCREENSAVEACTIVE, 0, &screensaver_active, 0);
	SystemParametersInfo(SPI_SETSCREENSAVEACTIVE, FALSE, NULL, 0);

	fb_win32.hinstance = (HINSTANCE)GetModuleHandle(NULL);
	fb_win32.window_title = title;
	strcpy( fb_win32.window_class, WINDOW_CLASS_PREFIX );
	strncat( fb_win32.window_class, fb_win32.window_title, WINDOW_TITLE_SIZE + sizeof(WINDOW_CLASS_PREFIX) - 1 );
	fb_win32.w = w;
	fb_win32.h = h;
	fb_win32.depth = depth;
	fb_win32.flags = flags;
	fb_win32.refresh_rate = refresh_rate;
	fb_win32.wndclass.lpfnWndProc = fb_hWin32WinProc;
	fb_win32.wndclass.lpszClassName = fb_win32.window_class;
	fb_win32.wndclass.hInstance = fb_win32.hinstance;
	fb_win32.wndclass.hCursor = LoadCursor(0, IDC_ARROW);
	fb_win32.wndclass.hIcon = LoadIcon(fb_win32.hinstance, "FB_PROGRAM_ICON");
	if (!fb_win32.wndclass.hIcon)
		fb_win32.wndclass.hIcon = LoadIcon(NULL, IDI_APPLICATION);
	fb_win32.wndclass.style = CS_VREDRAW | CS_HREDRAW | (flags & DRIVER_OPENGL ? CS_OWNDC : 0);
	RegisterClass(&fb_win32.wndclass);

	mouse_buttons = mouse_wheel = 0;
	fb_win32.is_running = TRUE;

	if (!(flags & DRIVER_OPENGL)) {
		InitializeCriticalSection(&update_lock);
        events[0] = CreateEvent(NULL, FALSE, FALSE, NULL);
#ifdef TARGET_WIN32
        events[1] = (HANDLE)_beginthread(fb_win32.thread, 0, events[0]);
#else
        {
            DWORD dwThreadId;
            events[1] = CreateThread( NULL,
                                      0,
                                      (LPTHREAD_START_ROUTINE) fb_win32.thread,
                                      events[0],
                                      0,
                                      &dwThreadId );
            assert( events[1]!=INVALID_HANDLE_VALUE );
        }
#endif
		result = WaitForMultipleObjects(2, events, FALSE, INFINITE);
		CloseHandle(events[0]);
		handle = events[1];
		if (result != WAIT_OBJECT_0)
			return -1;

		SetThreadPriority(handle, THREAD_PRIORITY_ABOVE_NORMAL);
	}
	else
		handle = NULL;

	return 0;
}


/*:::::*/
void fb_hWin32Exit(void)
{
	if (!fb_win32.is_running)
		return;
	fb_win32.is_running = FALSE;
	if (handle) {
		WaitForSingleObject(handle, INFINITE);
		DeleteCriticalSection(&update_lock);
	}
	SystemParametersInfo(SPI_SETSCREENSAVEACTIVE, screensaver_active, NULL, 0);
	UnregisterClass(fb_win32.window_class, fb_win32.hinstance);
}


/*:::::*/
void fb_hWin32Lock(void)
{
	EnterCriticalSection(&update_lock);
}


/*:::::*/
void fb_hWin32Unlock(void)
{
	LeaveCriticalSection(&update_lock);
}


/*:::::*/
void fb_hWin32SetPalette(int index, int r, int g, int b)
{
	fb_win32.palette[index].peRed = r;
	fb_win32.palette[index].peGreen = g;
	fb_win32.palette[index].peBlue = b;
	fb_win32.palette[index].peFlags = PC_NOCOLLAPSE;
	fb_win32.is_palette_changed = TRUE;
}


/*:::::*/
void fb_hWin32WaitVSync(void)
{
	Sleep(1000 / (fb_mode->refresh_rate ? fb_mode->refresh_rate : 60));
}


/*:::::*/
int fb_hWin32GetMouse(int *x, int *y, int *z, int *buttons)
{
	POINT point;

	if (!fb_win32.is_active)
		return -1;

	GetCursorPos(&point);

	if (fb_win32.flags & DRIVER_FULLSCREEN) {
		*x = MID(0, point.x, fb_win32.w - 1);
		*y = MID(0, point.y, fb_win32.h - 1);
	}
	else {
		ScreenToClient(fb_win32.wnd, &point);
		if ((point.x < 0) || (point.x >= fb_win32.w) || (point.y < 0) || (point.y >= fb_win32.h)) {
			mouse_buttons = 0;
			return -1;
		}
		else {
			*x = point.x;
			*y = point.y;
		}
	}
	*z = mouse_wheel;
	*buttons = mouse_buttons;

	return 0;
}


/*:::::*/
void fb_hWin32SetMouse(int x, int y, int cursor)
{
	POINT point;

	if (x >= 0) {
		point.x = MID(0, x, fb_win32.w - 1);
		point.y = MID(0, y, fb_win32.h - 1);
		if (!(fb_win32.flags & DRIVER_FULLSCREEN))
			ClientToScreen(fb_win32.wnd, &point);
		SetCursorPos(point.x, point.y);
	}

	if ((cursor == 0) && (cursor_shown)) {
		PostMessage(fb_win32.wnd, msg_cursor, FALSE, 0);
		cursor_shown = FALSE;
	}
	else if ((cursor > 0) && (!cursor_shown)) {
		PostMessage(fb_win32.wnd, msg_cursor, TRUE, 0);
		cursor_shown = TRUE;
	}
}


/*:::::*/
void fb_hWin32SetWindowTitle(char *title)
{
	SetWindowText(fb_win32.wnd, title);
}


/*:::::*/
void fb_hScreenInfo(int *width, int *height, int *depth, int *refresh)
{
	HDC hdc;

	hdc = GetDC(NULL);
	*width = GetDeviceCaps(hdc, HORZRES);
	*height = GetDeviceCaps(hdc, VERTRES);
	*depth = GetDeviceCaps(hdc, BITSPIXEL);
	*refresh = GetDeviceCaps(hdc, VREFRESH);
	ReleaseDC(NULL, hdc);
}

