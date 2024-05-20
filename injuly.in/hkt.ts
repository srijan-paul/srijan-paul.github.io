type Tagged<T, V = undefined>
	= V extends undefined
	? { tag: T }
	: { tag: T, value: V }

type Keypress = Tagged<'keypress', { keyCode: number }>
type Click = Tagged<'click', { x: number, y: number }>
type Hover = Tagged<'hover'>

type UiEvent = Click | Keypress | Hover

function serialize(event: UiEvent): string {
	switch (event.tag) {
		case 'click':
			return `Click at ${event.value.x}, ${event.value.y}`
		case 'keypress':
			return `Key press: ${event.value.keyCode}`
		case 'hover':
			return 'Hover'
	}
}

