import React from 'react';

export default React.createClass({

    render: function () {

        return (
            <li>
                {this.props.data.chordName}
                {this.props.data.content}
            </li>
        );

    }

});
