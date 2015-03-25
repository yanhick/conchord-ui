import React from 'react';

export default React.createClass({

    render: function () {
        return (
            <li>
                e:{this.props.data.notes.e}
                B:{this.props.data.notes.B}
                G:{this.props.data.notes.G}
                D:{this.props.data.notes.D}
                A:{this.props.data.notes.A}
                E:{this.props.data.notes.E}
            </li>
        );
    }

});

